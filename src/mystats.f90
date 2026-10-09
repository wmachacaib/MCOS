
module user_stats

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  
use decomp_2d_constants, only : mytype
use decomp_2d, only : DECOMP_INFO
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

implicit none

interface update_user_stats
   module procedure update_user_stats_non_phi
   module procedure update_user_stats_oui_phi
end interface update_user_stats

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
interface update_spat
   ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   ! calculate variables at each time step *ms (mspatial) only for first, second and bugdets
   ! the goal is to calculate um, tke, budgets
   module procedure update_user_stats_spat
   module procedure update_user_stats_spat_oui_phi
   ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
end interface update_spat
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


logical, save :: bool_user_stat
integer, save :: beg_stat
TYPE(DECOMP_INFO) :: decomp_user_stats
integer, save :: xst1, xst2, xst3
integer, save :: xen1, xen2, xen3

! Ordre 1
real(mytype), save, allocatable, dimension(:,:,:) :: um,vm,wm
real(mytype), save, allocatable, dimension(:,:,:) :: dpdxm, dpdym, dpdzm
real(mytype), save, allocatable, dimension(:,:,:) :: dudym, dvdym, dwdym
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dudxm, dvdxm, dwdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dudzm, dvdzm, dwdzm
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dudyym, dvdyym, dwdyym

! Ordre 2
real(mytype), save, allocatable, dimension(:,:,:) :: uum,vvm,wwm
real(mytype), save, allocatable, dimension(:,:,:) :: uvm,uwm,vwm
real(mytype), save, allocatable, dimension(:,:,:) :: udpdxm, udpdym, udpdzm
real(mytype), save, allocatable, dimension(:,:,:) :: vdpdxm, vdpdym, vdpdzm
real(mytype), save, allocatable, dimension(:,:,:) :: wdpdxm, wdpdym, wdpdzm

! Ordre 3
real(mytype), save, allocatable, dimension(:,:,:) :: uuum, uvvm, uwwm
real(mytype), save, allocatable, dimension(:,:,:) :: vuum, vvvm, vwwm
real(mytype), save, allocatable, dimension(:,:,:) :: wuum, wvvm, wwwm
real(mytype), save, allocatable, dimension(:,:,:) :: uvwm

! Epsilon pour tous les Rij
real(mytype), save, allocatable, dimension(:,:,:) :: dudx2m, dudy2m, dudz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dvdx2m, dvdy2m, dvdz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dwdx2m, dwdy2m, dwdz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdxm, dudwdxm, dvdwdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdym, dudwdym, dvdwdym
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdzm, dudwdzm, dvdwdzm


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! Ordre 1
real(mytype), save, allocatable, dimension(:,:,:) :: ums,vms,wms
real(mytype), save, allocatable, dimension(:,:,:) :: dpdxms, dpdyms, dpdzms
real(mytype), save, allocatable, dimension(:,:,:) :: dudyms, dvdyms, dwdyms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dudtms, dvdtms, dwdtms
real(mytype), save, allocatable, dimension(:,:,:) :: dudxms, dvdxms, dwdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dudzms, dvdzms, dwdzms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dudyyms, dvdyyms, dwdyyms

! Ordre 2
real(mytype), save, allocatable, dimension(:,:,:) :: uums,vvms,wwms
real(mytype), save, allocatable, dimension(:,:,:) :: uvms,uwms,vwms
real(mytype), save, allocatable, dimension(:,:,:) :: udpdxms, udpdyms, udpdzms
real(mytype), save, allocatable, dimension(:,:,:) :: vdpdxms, vdpdyms, vdpdzms
real(mytype), save, allocatable, dimension(:,:,:) :: wdpdxms, wdpdyms, wdpdzms

! Ordre 3
real(mytype), save, allocatable, dimension(:,:,:) :: uuums, uvvms, uwwms
real(mytype), save, allocatable, dimension(:,:,:) :: vuums, vvvms, vwwms
real(mytype), save, allocatable, dimension(:,:,:) :: wuums, wvvms, wwwms
real(mytype), save, allocatable, dimension(:,:,:) :: uvwms

! Epsilon pour tous les Rij
real(mytype), save, allocatable, dimension(:,:,:) :: dudx2ms, dudy2ms, dudz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdx2ms, dvdy2ms, dvdz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dwdx2ms, dwdy2ms, dwdz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdxms, dudwdxms, dvdwdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdyms, dudwdyms, dvdwdyms
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdzms, dudwdzms, dvdwdzms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



! Budget epsilon
! P0 : Production par convection avec vitesse moyenne, ok, cf ci-dessus
! P1 : Production par gradient vitesse moyenne : dUidXj*dUkdXj, ok, cf ci-dessus
! P2 : Production mixte, on a déjà les dudx², dudy², dudz², ajout termes croisés :
  ! uu, vv, ww
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdudym, dudxdudzm, dudydudzm
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxdvdym, dvdxdvdzm, dvdydvdzm
real(mytype), save, allocatable, dimension(:,:,:) :: dwdxdwdym, dwdxdwdzm, dwdydwdzm
  ! uv
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdvdym, dudydvdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdvdzm, dudzdvdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dudydvdzm, dudzdvdym
  ! uw
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdwdym, dudydwdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdwdzm, dudzdwdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dudydwdzm, dudzdwdym
  ! vw
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxdwdym, dvdydwdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxdwdzm, dvdzdwdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dvdydwdzm, dvdzdwdym

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ! uu, vv, ww
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdudyms, dudxdudzms, dudydudzms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxdvdyms, dvdxdvdzms, dvdydvdzms
real(mytype), save, allocatable, dimension(:,:,:) :: dwdxdwdyms, dwdxdwdzms, dwdydwdzms
  ! uv
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdvdyms, dudydvdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdvdzms, dudzdvdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dudydvdzms, dudzdvdyms
  ! uw
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdwdyms, dudydwdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dudxdwdzms, dudzdwdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dudydwdzms, dudzdwdyms
  ! vw
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxdwdyms, dvdydwdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxdwdzms, dvdzdwdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdydwdzms, dvdzdwdyms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

! P3 : Production du gradient (hessienne de la vitesse), ajout des termes manquants :
real(mytype), save, allocatable, dimension(:,:,:) :: vdudxm, vdudym, vdudzm
real(mytype), save, allocatable, dimension(:,:,:) :: vdvdxm, vdvdym, vdvdzm
real(mytype), save, allocatable, dimension(:,:,:) :: vdwdxm, vdwdym, vdwdzm

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! P3 : Production du gradient (hessienne de la vitesse), ajout des termes manquants :
real(mytype), save, allocatable, dimension(:,:,:) :: vdudxms, vdudyms, vdudzms
real(mytype), save, allocatable, dimension(:,:,:) :: vdvdxms, vdvdyms, vdvdzms
real(mytype), save, allocatable, dimension(:,:,:) :: vdwdxms, vdwdyms, vdwdzms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


! P4 : Production turbulente, produits triples
  ! EpsXX
real(mytype), save, allocatable, dimension(:,:,:) :: dudx_dududxm, dudx_dududym, dudx_dududzm!dudx_eps11
real(mytype), save, allocatable, dimension(:,:,:) :: dudy_dudvdxm, dudy_dudvdym, dudy_dudvdzm!dudy_eps12
real(mytype), save, allocatable, dimension(:,:,:) :: dudz_dudwdxm, dudz_dudwdym, dudz_dudwdzm!dudz_eps13
  ! EpsYY
real(mytype), save, allocatable, dimension(:,:,:) :: dvdx_dvdudxm, dvdx_dvdudym, dvdx_dvdudzm!dvdx_eps12
real(mytype), save, allocatable, dimension(:,:,:) :: dvdy_dvdvdxm, dvdy_dvdvdym, dvdy_dvdvdzm!dvdy_eps22
real(mytype), save, allocatable, dimension(:,:,:) :: dvdz_dvdwdxm, dvdz_dvdwdym, dvdz_dvdwdzm!dvdz_eps23
  ! EpsZZ
real(mytype), save, allocatable, dimension(:,:,:) :: dwdx_dwdudxm, dwdx_dwdudym, dwdx_dwdudzm!dwdx_eps13
real(mytype), save, allocatable, dimension(:,:,:) :: dwdy_dwdvdxm, dwdy_dwdvdym, dwdy_dwdvdzm!dwdy_eps23
real(mytype), save, allocatable, dimension(:,:,:) :: dwdz_dwdwdxm, dwdz_dwdwdym, dwdz_dwdwdzm!dwdz_eps33
  ! EpsXY
real(mytype), save, allocatable, dimension(:,:,:) :: dudx_dudvdxm, dudx_dudvdym, dudx_dudvdzm!dudx_eps21
real(mytype), save, allocatable, dimension(:,:,:) :: dudy_dvdvdxm, dudy_dvdvdym, dudy_dvdvdzm!dudy_eps22
real(mytype), save, allocatable, dimension(:,:,:) :: dudz_dvdwdxm, dudz_dvdwdym, dudz_dvdwdzm!dudz_eps23
real(mytype), save, allocatable, dimension(:,:,:) :: dvdx_dududxm, dvdx_dududym, dvdx_dududzm!dvdx_eps11
real(mytype), save, allocatable, dimension(:,:,:) :: dvdy_dudvdxm, dvdy_dudvdym, dvdy_dudvdzm!dvdy_eps12
real(mytype), save, allocatable, dimension(:,:,:) :: dvdz_dudwdxm, dvdz_dudwdym, dvdz_dudwdzm!dvdz_eps13
! Turbulent transport, ajout des termes NON-NULS :
real(mytype), save, allocatable, dimension(:,:,:) :: vdudx2m, vdudy2m, vdudz2m ! EpsXX
real(mytype), save, allocatable, dimension(:,:,:) :: vdvdx2m, vdvdy2m, vdvdz2m ! EpsYY
real(mytype), save, allocatable, dimension(:,:,:) :: vdwdx2m, vdwdy2m, vdwdz2m ! EpsZZ
real(mytype), save, allocatable, dimension(:,:,:) :: vdudvdxm, vdudvdym, vdudvdzm ! EpsXY
! Pressure transport, ajout des termes NON-NULS
real(mytype), save, allocatable, dimension(:,:,:) :: dpdvdxm, dpdvdym, dpdvdzm ! EpsYY
real(mytype), save, allocatable, dimension(:,:,:) :: dpdudxm, dpdudym, dpdudzm ! EpsXY
! Pressure production sur EpsXY
real(mytype), save, allocatable, dimension(:,:,:) :: dpdxdudyxm, dpdydudyym, dpdzdudyzm
real(mytype), save, allocatable, dimension(:,:,:) :: dpdxdvdxxm, dpdydvdxym, dpdzdvdxzm
! Dissipation
  ! uu
real(mytype), save, allocatable, dimension(:,:,:) :: dudxx2m, dudyy2m, dudzz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dudxy2m, dudxz2m, dudyz2m
  ! vv
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxx2m, dvdyy2m, dvdzz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxy2m, dvdxz2m, dvdyz2m
  ! ww
real(mytype), save, allocatable, dimension(:,:,:) :: dwdxx2m, dwdyy2m, dwdzz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dwdxy2m, dwdxz2m, dwdyz2m
  ! uv
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdxx2m, dudvdyy2m, dudvdzz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdxy2m, dudvdxz2m, dudvdyz2m


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! Dissipation
  ! uu
real(mytype), save, allocatable, dimension(:,:,:) :: dudxx2ms, dudyy2ms, dudzz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dudxy2ms, dudxz2ms, dudyz2ms
  ! vv
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxx2ms, dvdyy2ms, dvdzz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdxy2ms, dvdxz2ms, dvdyz2ms
  ! ww
real(mytype), save, allocatable, dimension(:,:,:) :: dwdxx2ms, dwdyy2ms, dwdzz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dwdxy2ms, dwdxz2ms, dwdyz2ms
  ! uv
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdxx2ms, dudvdyy2ms, dudvdzz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dudvdxy2ms, dudvdxz2ms, dudvdyz2ms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


! Scalaire
! Ordre 1
real(mytype), save, allocatable, dimension(:,:,:) :: phim
real(mytype), save, allocatable, dimension(:,:,:) :: dphidym
!xxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dphidxm, dphidzm
!xxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dphidyym
real(mytype), save, allocatable, dimension(:,:,:) :: dphidyyym
! Ordre 2
real(mytype), save, allocatable, dimension(:,:,:) :: uphim, vphim, wphim, phiphim
real(mytype), save, allocatable, dimension(:,:,:) :: phidpdxm, phidpdym, phidpdzm
!
real(mytype), save, allocatable, dimension(:,:,:) :: dphidx2m, dphidy2m, dphidz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dudphidxm, dvdphidxm, dwdphidxm
real(mytype), save, allocatable, dimension(:,:,:) :: dudphidym, dvdphidym, dwdphidym
real(mytype), save, allocatable, dimension(:,:,:) :: dudphidzm, dvdphidzm, dwdphidzm
real(mytype), save, allocatable, dimension(:,:,:) :: udtdxxm, udtdyym, udtdzzm
real(mytype), save, allocatable, dimension(:,:,:) :: vdtdxxm, vdtdyym, vdtdzzm
real(mytype), save, allocatable, dimension(:,:,:) :: wdtdxxm, wdtdyym, wdtdzzm
! Ordre 3
real(mytype), save, allocatable, dimension(:,:,:) :: uphi2m,vphi2m,wphi2m
real(mytype), save, allocatable, dimension(:,:,:) :: phiuum,phivvm,phiwwm
real(mytype), save, allocatable, dimension(:,:,:) :: phiuvm,phiuwm,phivwm


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! Ordre 1
real(mytype), save, allocatable, dimension(:,:,:) :: phims
real(mytype), save, allocatable, dimension(:,:,:) :: dphidyms
!xxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dphidxms, dphidzms
!xxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dphidyyms
real(mytype), save, allocatable, dimension(:,:,:) :: dphidyyyms
! Ordre 2
real(mytype), save, allocatable, dimension(:,:,:) :: uphims, vphims, wphims, phiphims
real(mytype), save, allocatable, dimension(:,:,:) :: phidpdxms, phidpdyms, phidpdzms
!
real(mytype), save, allocatable, dimension(:,:,:) :: dphidx2ms, dphidy2ms, dphidz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dudphidxms, dvdphidxms, dwdphidxms
real(mytype), save, allocatable, dimension(:,:,:) :: dudphidyms, dvdphidyms, dwdphidyms
real(mytype), save, allocatable, dimension(:,:,:) :: dudphidzms, dvdphidzms, dwdphidzms
real(mytype), save, allocatable, dimension(:,:,:) :: udtdxxms, udtdyyms, udtdzzms
real(mytype), save, allocatable, dimension(:,:,:) :: vdtdxxms, vdtdyyms, vdtdzzms
real(mytype), save, allocatable, dimension(:,:,:) :: wdtdxxms, wdtdyyms, wdtdzzms
! Ordre 3
real(mytype), save, allocatable, dimension(:,:,:) :: uphi2ms,vphi2ms,wphi2ms
real(mytype), save, allocatable, dimension(:,:,:) :: phiuums,phivvms,phiwwms
real(mytype), save, allocatable, dimension(:,:,:) :: phiuvms,phiuwms,phivwms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
! Budget epsilon thermique
! P0 : Production par convection avec vitesse moyenne, OK
! P1 : Production par gradient vitesse moyenne : dUidXj*dTdXj, ok, cf ci-dessus
! P2 : Production mixte, gradU*gradT, ajout des termes croisés
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdtdym, dtdxdtdzm, dtdydtdzm ! TT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdudym, dtdydudxm ! uT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdudzm, dtdzdudxm
real(mytype), save, allocatable, dimension(:,:,:) :: dtdydudzm, dtdzdudym
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdvdym, dtdydvdxm ! vT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdvdzm, dtdzdvdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dtdydvdzm, dtdzdvdym
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdwdym, dtdydwdxm ! wT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdwdzm, dtdzdwdxm
real(mytype), save, allocatable, dimension(:,:,:) :: dtdydwdzm, dtdzdwdym
! P3 : Production du gradient (hessienne vitesse et temp.), ajout termes manquants :
real(mytype), save, allocatable, dimension(:,:,:) :: vdtdxm, vdtdym, vdtdzm


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdtdyms, dtdxdtdzms, dtdydtdzms ! TT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdudyms, dtdydudxms ! uT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdudzms, dtdzdudxms
real(mytype), save, allocatable, dimension(:,:,:) :: dtdydudzms, dtdzdudyms
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdvdyms, dtdydvdxms ! vT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdvdzms, dtdzdvdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dtdydvdzms, dtdzdvdyms
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdwdyms, dtdydwdxms ! wT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxdwdzms, dtdzdwdxms
real(mytype), save, allocatable, dimension(:,:,:) :: dtdydwdzms, dtdzdwdyms
! P3 : Production du gradient (hessienne vitesse et temp.), ajout termes manquants :
real(mytype), save, allocatable, dimension(:,:,:) :: vdtdxms, vdtdyms, vdtdzms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! P4 : Production turbulente, produits triples
  ! EpsXT
real(mytype), save, allocatable, dimension(:,:,:) :: dudx_dudtdxm, dudx_dudtdym, dudx_dudtdzm!dudx_epsxT
real(mytype), save, allocatable, dimension(:,:,:) :: dudy_dvdtdxm, dudy_dvdtdym, dudy_dvdtdzm!dudy_epsyT
real(mytype), save, allocatable, dimension(:,:,:) :: dudz_dwdtdxm, dudz_dwdtdym, dudz_dwdtdzm!dudz_epszT
real(mytype), save, allocatable, dimension(:,:,:) ::               dtdx_dududym, dtdx_dududzm!dtdx_epsxx
real(mytype), save, allocatable, dimension(:,:,:) :: dtdy_dudvdxm,               dtdy_dudvdzm!dtdy_epsxy
real(mytype), save, allocatable, dimension(:,:,:) :: dtdz_dudwdxm, dtdz_dudwdym              !dtdz_epsxz
  ! EpxYT
real(mytype), save, allocatable, dimension(:,:,:) :: dvdx_dudtdxm, dvdx_dudtdym, dvdx_dudtdzm!dvdx_epsxT
real(mytype), save, allocatable, dimension(:,:,:) :: dvdy_dvdtdxm, dvdy_dvdtdym, dvdy_dvdtdzm!dvdy_epsyT
real(mytype), save, allocatable, dimension(:,:,:) :: dvdz_dwdtdxm, dvdz_dwdtdym, dvdz_dwdtdzm!dvdz_epszT
real(mytype), save, allocatable, dimension(:,:,:) ::               dtdx_dudvdym, dtdx_dudvdzm!dtdx_epsxy
real(mytype), save, allocatable, dimension(:,:,:) :: dtdy_dvdvdxm,               dtdy_dvdvdzm!dtdy_epsyy
real(mytype), save, allocatable, dimension(:,:,:) :: dtdz_dvdwdxm, dtdz_dvdwdym              !dtdz_epsyz
  ! EpsTT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdx_dudtdxm, dtdx_dudtdym, dtdx_dudtdzm!dtdx_epsxT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdy_dvdtdxm, dtdy_dvdtdym, dtdy_dvdtdzm!dtdy_epsyT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdz_dwdtdxm, dtdz_dwdtdym, dtdz_dwdtdzm!dtdz_epszT
! Turbulent transport
real(mytype), save, allocatable, dimension(:,:,:) :: vdudtdxm, vdudtdym, vdudtdzm!v_epsxT
real(mytype), save, allocatable, dimension(:,:,:) :: vdvdtdxm, vdvdtdym, vdvdtdzm!v_epsyT
real(mytype), save, allocatable, dimension(:,:,:) :: vdtdx2m, vdtdy2m, vdtdz2m!v_epsTT
! Pressure transport + production
real(mytype), save, allocatable, dimension(:,:,:) :: dpdtdxm, dpdtdym, dpdtdzm
real(mytype), save, allocatable, dimension(:,:,:) :: dpdtdxxm, dpdtdxym, dpdtdxzm ! EpsXT
real(mytype), save, allocatable, dimension(:,:,:) :: dpdtdyxm, dpdtdyym, dpdtdyzm ! EpsYT
! Dissipation
  ! xT
real(mytype), save, allocatable, dimension(:,:,:) :: dudtdxxm, dudtdyym, dudtdzzm
real(mytype), save, allocatable, dimension(:,:,:) :: dudtdxym, dudtdxzm, dudtdyzm
real(mytype), save, allocatable, dimension(:,:,:) :: dudx_dtdxxxm, dudx_dtdyxxm, dudx_dtdzxxm
real(mytype), save, allocatable, dimension(:,:,:) :: dudy_dtdxyym, dudy_dtdyyym, dudy_dtdzyym
real(mytype), save, allocatable, dimension(:,:,:) :: dudz_dtdxzzm, dudz_dtdyzzm, dudz_dtdzzzm
  ! yT
real(mytype), save, allocatable, dimension(:,:,:) :: dvdtdxxm, dvdtdyym, dvdtdzzm
real(mytype), save, allocatable, dimension(:,:,:) :: dvdtdxym, dvdtdxzm, dvdtdyzm
real(mytype), save, allocatable, dimension(:,:,:) :: dvdx_dtdxxxm, dvdx_dtdyxxm, dvdx_dtdzxxm
real(mytype), save, allocatable, dimension(:,:,:) :: dvdy_dtdxyym, dvdy_dtdyyym, dvdy_dtdzyym
real(mytype), save, allocatable, dimension(:,:,:) :: dvdz_dtdxzzm, dvdz_dtdyzzm, dvdz_dtdzzzm
  ! TT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxx2m, dtdyy2m, dtdzz2m
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxy2m, dtdxz2m, dtdyz2m

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! xT
real(mytype), save, allocatable, dimension(:,:,:) :: dudtdxxms, dudtdyyms, dudtdzzms
real(mytype), save, allocatable, dimension(:,:,:) :: dudtdxyms, dudtdxzms, dudtdyzms
real(mytype), save, allocatable, dimension(:,:,:) :: dudx_dtdxxxms, dudx_dtdyxxms, dudx_dtdzxxms
real(mytype), save, allocatable, dimension(:,:,:) :: dudy_dtdxyyms, dudy_dtdyyyms, dudy_dtdzyyms
real(mytype), save, allocatable, dimension(:,:,:) :: dudz_dtdxzzms, dudz_dtdyzzms, dudz_dtdzzms
  ! yT
real(mytype), save, allocatable, dimension(:,:,:) :: dvdtdxxms, dvdtdyyms, dvdtdzzms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdtdxyms, dvdtdxzms, dvdtdyzms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdx_dtdxxxms, dvdx_dtdyxxms, dvdx_dtdzxxms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdy_dtdxyyms, dvdy_dtdyyyms, dvdy_dtdzyyms
real(mytype), save, allocatable, dimension(:,:,:) :: dvdz_dtdxzzms, dvdz_dtdyzzms, dvdz_dtdzzms
  ! TT
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxx2ms, dtdyy2ms, dtdzz2ms
real(mytype), save, allocatable, dimension(:,:,:) :: dtdxy2ms, dtdxz2ms, dtdyz2ms
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! Pression
real(mytype), save, allocatable, dimension(:,:,:) :: presm, pres2m
!real(mytype), save, allocatable, dimension(:,:,:) :: pp3m, pp32m

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
real(mytype), save, allocatable, dimension(:,:,:) :: presms, pres2ms
!real(mytype), save, allocatable, dimension(:,:,:) :: pp3m, pp32m
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
contains

subroutine allocate_user_stats(nx_user, ny_user, nz_user, phG,ph1,ph2,ph3,ph4)

use decomp_2d, only : decomp_info_init, DECOMP_INFO, get_decomp_info
use param, only : iscalar

implicit none

integer, intent(in) :: nx_user, ny_user, nz_user
TYPE(DECOMP_INFO), intent(in) :: phG,ph1,ph2,ph3,ph4
TYPE(DECOMP_INFO) :: decomp_main

call decomp_info_init(nx_user, ny_user, nz_user, decomp_user_stats)

xst1=decomp_user_stats%xst(1)
xst2=decomp_user_stats%xst(2)
xst3=decomp_user_stats%xst(3)
!xen1=decomp_user_stats%xst(1)!///esto no estará mal?? podría ser xen(1)???
xen1=decomp_user_stats%xen(1)
xen2=decomp_user_stats%xen(2)
xen3=decomp_user_stats%xen(3)
!print *,'we are here', xst1,xst2,xen1, xen2 in parallel form

allocate(um     (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vm     (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wm     (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdxm  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdym  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdzm  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudym  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdym  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdym  (xst1:xen1,xst2:xen2,xst3:xen3))
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
allocate(dudxm  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxm  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxm  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzm  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdzm  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdzm  (xst1:xen1,xst2:xen2,xst3:xen3))
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
allocate(dudyym (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdyym (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdyym (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uum    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vvm    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wwm    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uvm    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uwm    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vwm    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(udpdxm (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(udpdym (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(udpdzm (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdpdxm (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdpdym (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdpdzm (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wdpdxm (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wdpdym (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wdpdzm (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uuum   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uvvm   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uwwm   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vuum   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vvvm   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vwwm   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wuum   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wvvm   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wwwm   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uvwm   (xst1:xen1,xst2:xen2,xst3:xen3))


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
allocate(ums     (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vms     (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wms     (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdxms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdyms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdzms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudyms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdyms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdyms  (xst1:xen1,xst2:xen2,xst3:xen3))
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
allocate(dudtms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdtms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdtms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdzms  (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdzms  (xst1:xen1,xst2:xen2,xst3:xen3))
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
allocate(dudyyms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdyyms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdyyms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uums    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vvms    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wwms    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uvms    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uwms    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vwms    (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(udpdxms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(udpdyms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(udpdzms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdpdxms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdpdyms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdpdzms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wdpdxms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wdpdyms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wdpdzms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uuums   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uvvms   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uwwms   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vuums   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vvvms   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vwwms   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wuums   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wvvms   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(wwwms   (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(uvwms   (xst1:xen1,xst2:xen2,xst3:xen3))
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



allocate(dudx2m (xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXX
allocate(dudy2m (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudz2m (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdx2m (xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsYY
allocate(dvdy2m (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdz2m (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdx2m (xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsZZ
allocate(dwdy2m (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdz2m (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXY
allocate(dudvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudwdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXZ
allocate(dudwdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdwdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsYZ
allocate(dvdwdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
allocate(dudx2ms (xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXX
allocate(dudy2ms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudz2ms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdx2ms (xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsYY
allocate(dvdy2ms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdz2ms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdx2ms (xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsZZ
allocate(dwdy2ms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdz2ms (xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdxms(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXY
allocate(dudvdyms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudwdxms(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXZ
allocate(dudwdyms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdwdxms(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsYZ
allocate(dvdwdyms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

allocate(dudxdudym(xst1:xen1,xst2:xen2,xst3:xen3)) ! uu
allocate(dudxdudzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudydudzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxdvdym(xst1:xen1,xst2:xen2,xst3:xen3)) ! vv
allocate(dvdxdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdydvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxdwdym(xst1:xen1,xst2:xen2,xst3:xen3)) ! ww
allocate(dwdxdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdydwdzm(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dudxdvdym(xst1:xen1,xst2:xen2,xst3:xen3)) ! uv
allocate(dudydvdxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzdvdxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudydvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzdvdym(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dudxdwdym(xst1:xen1,xst2:xen2,xst3:xen3)) ! uw
allocate(dudydwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzdwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudydwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzdwdym(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dvdxdwdym(xst1:xen1,xst2:xen2,xst3:xen3)) ! vw
allocate(dvdydwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdzdwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdydwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdzdwdym(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(vdudxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdudym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdudzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdvdxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdwdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
allocate(dudxdudyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! uu
allocate(dudxdudzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudydudzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxdvdyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! vv
allocate(dvdxdvdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdydvdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxdwdyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! ww
allocate(dwdxdwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdydwdzms(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dudxdvdyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! uv
allocate(dudydvdxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxdvdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzdvdxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudydvdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzdvdyms(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dudxdwdyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! uw
allocate(dudydwdxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxdwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzdwdxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudydwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzdwdyms(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dvdxdwdyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! vw
allocate(dvdydwdxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxdwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdzdwdxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdydwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdzdwdyms(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(vdudxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdudyms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdudzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdvdxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdvdyms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdvdzms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdwdxms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdwdyms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

allocate(dudx_dududxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dudx_eps11
allocate(dudx_dududym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudx_dududzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudy_dudvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dudy_eps12
allocate(dudy_dudvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudy_dudvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudz_dudwdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dudz_eps13
allocate(dudz_dudwdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudz_dudwdzm(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dvdx_dvdudxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dvdx_eps12
allocate(dvdx_dvdudym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdx_dvdudzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdy_dvdvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dvdy_eps22
allocate(dvdy_dvdvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdy_dvdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdz_dvdwdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dvdz_eps23
allocate(dvdz_dvdwdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdz_dvdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dwdx_dwdudxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dwdx_eps13
allocate(dwdx_dwdudym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdx_dwdudzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdy_dwdvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dwdy_eps23
allocate(dwdy_dwdvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdy_dwdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdz_dwdwdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dwdz_eps33
allocate(dwdz_dwdwdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdz_dwdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dudx_dudvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dudx_eps21
allocate(dudx_dudvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudx_dudvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudy_dvdvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dudy_eps22
allocate(dudy_dvdvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudy_dvdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudz_dvdwdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dudz_eps23
allocate(dudz_dvdwdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudz_dvdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdx_dududxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dvdx_eps11
allocate(dvdx_dududym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdx_dududzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdy_dudvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dvdy_eps12
allocate(dvdy_dudvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdy_dudvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdz_dudwdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! dvdz_eps13
allocate(dvdz_dudwdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdz_dudwdzm(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(vdudx2m(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXX
allocate(vdudy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdudz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdvdx2m(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsYY
allocate(vdvdy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdvdz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdwdx2m(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsZZ
allocate(vdwdy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdwdz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdudvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXY
allocate(vdudvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(vdudvdzm(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dpdvdxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsYY
allocate(dpdvdym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdudxm(xst1:xen1,xst2:xen2,xst3:xen3)) ! EpsXY
allocate(dpdudym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdudzm(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dpdxdudyxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdydudyym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdzdudyzm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdxdvdxxm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdydvdxym(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dpdzdvdxzm(xst1:xen1,xst2:xen2,xst3:xen3))

allocate(dudxx2m(xst1:xen1,xst2:xen2,xst3:xen3)) ! uu
allocate(dudyy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudyz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxx2m(xst1:xen1,xst2:xen2,xst3:xen3)) ! vv
allocate(dvdyy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdzz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdyz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxx2m(xst1:xen1,xst2:xen2,xst3:xen3)) ! ww
allocate(dwdyy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdzz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdyz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdxx2m(xst1:xen1,xst2:xen2,xst3:xen3)) ! uv
allocate(dudvdyy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdzz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdxy2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdxz2m(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdyz2m(xst1:xen1,xst2:xen2,xst3:xen3))


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
allocate(dudxx2ms(xst1:xen1,xst2:xen2,xst3:xen3)) ! uu
allocate(dudyy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudzz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudxz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudyz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxx2ms(xst1:xen1,xst2:xen2,xst3:xen3)) ! vv
allocate(dvdyy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdzz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdxz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dvdyz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxx2ms(xst1:xen1,xst2:xen2,xst3:xen3)) ! ww
allocate(dwdyy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdzz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdxz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dwdyz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdxx2ms(xst1:xen1,xst2:xen2,xst3:xen3)) ! uv
allocate(dudvdyy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdzz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdxy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdxz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(dudvdyz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

um=0.; vm=0.; wm=0.;
dpdxm=0.; dpdym=0.; dpdzm=0.;
dudym=0.; dvdym=0.; dwdym=0.;
! xxxxxxxxxxxxxxxxxxxxxxxxxxx
dudxm=0.; dvdxm=0.; dwdxm=0.;
dudzm=0.; dvdzm=0.; dwdzm=0.;
! xxxxxxxxxxxxxxxxxxxxxxxxxxx
dudyym=0.; dvdyym=0.; dwdyym=0.;
uum=0.; vvm=0.; wwm=0.;
uvm=0.; uwm=0.; vwm=0.;
udpdxm=0.; udpdym=0.; udpdzm=0.;
vdpdxm=0.; vdpdym=0.; vdpdzm=0.;
wdpdxm=0.; wdpdym=0.; wdpdzm=0.;
uuum=0.; uvvm=0.; uwwm=0.;
vuum=0.; vvvm=0.; vwwm=0.;
wuum=0.; wvvm=0.; wwwm=0.
uvwm=0.;

dudx2m=0.; dudy2m=0.; dudz2m=0.;
dvdx2m=0.; dvdy2m=0.; dvdz2m=0.;
dwdx2m=0.; dwdy2m=0.; dwdz2m=0.;
dudvdxm=0.; dudwdxm=0.; dvdwdxm=0.;
dudvdym=0.; dudwdym=0.; dvdwdym=0.;
dudvdzm=0.; dudwdzm=0.; dvdwdzm=0.;

dudxdudym=0.; dudxdudzm=0.; dudydudzm=0.; ! uu
dvdxdvdym=0.; dvdxdvdzm=0.; dvdydvdzm=0.; ! vv
dwdxdwdym=0.; dwdxdwdzm=0.; dwdydwdzm=0.; ! ww

dudxdvdym=0.; dudydvdxm=0.; ! uv
dudxdvdzm=0.; dudzdvdxm=0.;
dudydvdzm=0.; dudzdvdym=0.;

dudxdwdym=0.; dudydwdxm=0.; ! uw
dudxdwdzm=0.; dudzdwdxm=0.;
dudydwdzm=0.; dudzdwdym=0.;

dvdxdwdym=0.; dvdydwdxm=0.; ! vw
dvdxdwdzm=0.; dvdzdwdxm=0.;
dvdydwdzm=0.; dvdzdwdym=0.;

vdudxm=0.; vdvdxm=0.; vdwdxm=0.;
vdudym=0.; vdvdym=0.; vdwdym=0.;
vdudzm=0.; vdvdzm=0.; vdwdzm=0.;

dudx_dududxm=0.; dudx_dududym=0.; dudx_dududzm=0.; ! dudx_eps11
dudy_dudvdxm=0.; dudy_dudvdym=0.; dudy_dudvdzm=0.; ! dudy_eps12
dudz_dudwdxm=0.; dudz_dudwdym=0.; dudz_dudwdzm=0.; ! dudz_eps13

dvdx_dvdudxm=0.; dvdx_dvdudym=0.; dvdx_dvdudzm=0.; ! dvdx_eps12
dvdy_dvdvdxm=0.; dvdy_dvdvdym=0.; dvdy_dvdvdzm=0.; ! dvdy_eps22
dvdz_dvdwdxm=0.; dvdz_dvdwdym=0.; dvdz_dvdwdzm=0.; ! dvdz_eps23

dwdx_dwdudxm=0.; dwdx_dwdudym=0.; dwdx_dwdudzm=0.; ! dwdx_eps13
dwdy_dwdvdxm=0.; dwdy_dwdvdym=0.; dwdy_dwdvdzm=0.; ! dwdy_eps23
dwdz_dwdwdxm=0.; dwdz_dwdwdym=0.; dwdz_dwdwdzm=0.; ! dwdz_eps33

dudx_dudvdxm=0.; dudx_dudvdym=0.; dudx_dudvdzm=0.; !dudx_eps21
dudy_dvdvdxm=0.; dudy_dvdvdym=0.; dudy_dvdvdzm=0.; !dudy_eps22
dudz_dvdwdxm=0.; dudz_dvdwdym=0.; dudz_dvdwdzm=0.; !dudz_eps23
dvdx_dududxm=0.; dvdx_dududym=0.; dvdx_dududzm=0.; !dvdx_eps11
dvdy_dudvdxm=0.; dvdy_dudvdym=0.; dvdy_dudvdzm=0.; !dvdy_eps12
dvdz_dudwdxm=0.; dvdz_dudwdym=0.; dvdz_dudwdzm=0.; !dvdz_eps13

vdudx2m=0.; vdudy2m=0.; vdudz2m=0.; ! EpsXX
vdvdx2m=0.; vdvdy2m=0.; vdvdz2m=0.; ! EpsYY
vdwdx2m=0.; vdwdy2m=0.; vdwdz2m=0.; ! EpsZZ
vdudvdxm=0.; vdudvdym=0.; vdudvdzm=0.; ! EpsXY

dpdvdxm=0.; dpdvdym=0.; dpdvdzm=0.; ! EpsYY
dpdudxm=0.; dpdudym=0.; dpdudzm=0.; ! EpsXY

dpdxdudyxm=0.; dpdydudyym=0.; dpdzdudyzm=0.;
dpdxdvdxxm=0.; dpdydvdxym=0.; dpdzdvdxzm=0.;

dudxx2m=0.; dudyy2m=0.; dudzz2m=0.; ! uu
dudxy2m=0.; dudxz2m=0.; dudyz2m=0.;
dvdxx2m=0.; dvdyy2m=0.; dvdzz2m=0.; ! vv
dvdxy2m=0.; dvdxz2m=0.; dvdyz2m=0.;
dwdxx2m=0.; dwdyy2m=0.; dwdzz2m=0.; ! ww
dwdxy2m=0.; dwdxz2m=0.; dwdyz2m=0.;
dudvdxx2m=0.; dudvdyy2m=0.; dudvdzz2m=0.; ! uv
dudvdxy2m=0.; dudvdxz2m=0.; dudvdyz2m=0.;


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
ums=0.; vms=0.; wms=0.;
dpdxms=0.; dpdyms=0.; dpdzms=0.;
dudyms=0.; dvdyms=0.; dwdyms=0.;
! xxxxxxxxxxxxxxxxxxxxxxxxxxx
dudtms=0.; dvdtms=0.; dwdtms=0.;
dudxms=0.; dvdxms=0.; dwdxms=0.;
dudzms=0.; dvdzms=0.; dwdzms=0.;
! xxxxxxxxxxxxxxxxxxxxxxxxxxx
dudyyms=0.; dvdyyms=0.; dwdyyms=0.;
uums=0.; vvms=0.; wwms=0.;
uvms=0.; uwms=0.; vwms=0.;
udpdxms=0.; udpdyms=0.; udpdzms=0.;
vdpdxms=0.; vdpdyms=0.; vdpdzms=0.;
wdpdxms=0.; wdpdyms=0.; wdpdzms=0.;
uuums=0.; uvvms=0.; uwwms=0.;
vuums=0.; vvvms=0.; vwwms=0.;
wuums=0.; wvvms=0.; wwwms=0.
uvwms=0.;

dudx2ms=0.; dudy2ms=0.; dudz2ms=0.;
dvdx2ms=0.; dvdy2ms=0.; dvdz2ms=0.;
dwdx2ms=0.; dwdy2ms=0.; dwdz2ms=0.;
dudvdxms=0.; dudwdxms=0.; dvdwdxms=0.;
dudvdyms=0.; dudwdyms=0.; dvdwdyms=0.;
dudvdzms=0.; dudwdzms=0.; dvdwdzms=0.;

dudxdudyms=0.; dudxdudzms=0.; dudydudzms=0.; ! uu
dvdxdvdyms=0.; dvdxdvdzms=0.; dvdydvdzms=0.; ! vv
dwdxdwdyms=0.; dwdxdwdzms=0.; dwdydwdzms=0.; ! ww

dudxdvdyms=0.; dudydvdxms=0.; ! uv
dudxdvdzms=0.; dudzdvdxms=0.;
dudydvdzms=0.; dudzdvdyms=0.;

dudxdwdyms=0.; dudydwdxms=0.; ! uw
dudxdwdzms=0.; dudzdwdxms=0.;
dudydwdzms=0.; dudzdwdyms=0.;

dvdxdwdyms=0.; dvdydwdxms=0.; ! vw
dvdxdwdzms=0.; dvdzdwdxms=0.;
dvdydwdzms=0.; dvdzdwdyms=0.;

vdudxms=0.; vdvdxms=0.; vdwdxms=0.;
vdudyms=0.; vdvdyms=0.; vdwdyms=0.;
vdudzms=0.; vdvdzms=0.; vdwdzms=0.;

!!!!!!!!
!!!!!!!!!111

dudxx2ms=0.; dudyy2ms=0.; dudzz2ms=0.; ! uu
dudxy2ms=0.; dudxz2ms=0.; dudyz2ms=0.;
dvdxx2ms=0.; dvdyy2ms=0.; dvdzz2ms=0.; ! vv
dvdxy2ms=0.; dvdxz2ms=0.; dvdyz2ms=0.;
dwdxx2ms=0.; dwdyy2ms=0.; dwdzz2ms=0.; ! ww
dwdxy2ms=0.; dwdxz2ms=0.; dwdyz2ms=0.;
dudvdxx2ms=0.; dudvdyy2ms=0.; dudvdzz2ms=0.; ! uv
dudvdxy2ms=0.; dudvdxz2ms=0.; dudvdyz2ms=0.;
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

! Scalaire
if (iscalar.eq.1) then
  ! Ordre 1
  allocate(phim(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidym(xst1:xen1,xst2:xen2,xst3:xen3))
!xxxxxxxxxxxxxxxxxxxxxxxxxxxx
  allocate(dphidxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidzm(xst1:xen1,xst2:xen2,xst3:xen3))
!xxxxxxxxxxxxxxxxxxxxxxxxxxxx
  allocate(dphidyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidyyym(xst1:xen1,xst2:xen2,xst3:xen3))
  ! Ordre 2
  allocate(uphim(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vphim(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wphim(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiphim(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phidpdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phidpdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phidpdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dphidx2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidy2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidz2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudphidxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdphidxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dwdphidxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudphidym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdphidym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dwdphidym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudphidzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdphidzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dwdphidzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(udtdxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(udtdyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(udtdzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wdtdxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wdtdyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wdtdzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  ! Ordre 3
  allocate(uphi2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vphi2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wphi2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiuum(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phivvm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiwwm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiuvm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiuwm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phivwm(xst1:xen1,xst2:xen2,xst3:xen3))
  ! P2
  allocate(dtdxdtdym(xst1:xen1,xst2:xen2,xst3:xen3)) ! TT
  allocate(dtdxdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdydtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdudym(xst1:xen1,xst2:xen2,xst3:xen3)) ! uT
  allocate(dtdydudxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdudzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdudxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdydudzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdudym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdvdym(xst1:xen1,xst2:xen2,xst3:xen3)) ! vT
  allocate(dtdydvdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdvdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdydvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdvdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdwdym(xst1:xen1,xst2:xen2,xst3:xen3)) ! wT
  allocate(dtdydwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdydwdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdwdym(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(vdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dudx_dudtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudx_dudtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudx_dudtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dvdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dvdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dvdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dwdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dwdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dwdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dtdx_dududym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdx_dududzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdy_dudvdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdy_dudvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdz_dudwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdz_dudwdym(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dvdx_dudtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdx_dudtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdx_dudtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dvdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dvdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dvdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dwdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dwdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dwdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dtdx_dudvdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdx_dudvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdy_dvdvdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdy_dvdvdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdz_dvdwdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdz_dvdwdym(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dtdx_dudtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdx_dudtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdx_dudtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdy_dvdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdy_dvdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdy_dvdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdz_dwdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdz_dwdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdz_dwdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(vdudtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdudtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdudtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdvdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdvdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdvdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdx2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdy2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdz2m(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dpdtdxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dpdtdym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dpdtdzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dpdtdxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dpdtdxym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dpdtdxzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dpdtdyxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dpdtdyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dpdtdyzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dudtdxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdxym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdxzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdyzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dudx_dtdxxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudx_dtdyxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudx_dtdzxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dtdxyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dtdyyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dtdzyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dtdxzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dtdyzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dtdzzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dvdtdxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdxym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdxzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdyzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dvdx_dtdxxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdx_dtdyxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdx_dtdzxxm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dtdxyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dtdyyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dtdzyym(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dtdxzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dtdyzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dtdzzzm(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  allocate(dtdxx2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdyy2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzz2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxy2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxz2m(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdyz2m(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  phim=0.;
  dphidym=0.;
!xxxxxxxxxxxxxxxxxxxxxxxxxx
  dphidxm=0.;
  dphidzm=0.;
!xxxxxxxxxxxxxxxxxxxxxxxxxx
  dphidyym=0.;
  dphidyyym=0.;
  uphim=0.; vphim=0.; wphim=0.; phiphim=0.;
  phidpdxm=0.; phidpdym=0.; phidpdzm=0.;
  dphidx2m=0.; dphidy2m=0.; dphidz2m=0.;
  dudphidxm=0.; dvdphidxm=0.; dwdphidxm=0.;
  dudphidym=0.; dvdphidym=0.; dwdphidym=0.;
  dudphidzm=0.; dvdphidzm=0.; dwdphidzm=0.;
  udtdxxm=0.; udtdyym=0.; udtdzzm=0.;
  vdtdxxm=0.; vdtdyym=0.; vdtdzzm=0.;
  wdtdxxm=0.; wdtdyym=0.; wdtdzzm=0.;
  uphi2m=0.; vphi2m=0.; wphi2m=0.;
  phiuum=0.; phivvm=0.; phiwwm=0.;
  phiuvm=0.; phiuwm=0.; phivwm=0.;
  !
  dtdxdtdym=0.; dtdxdtdzm=0.; dtdydtdzm=0.; ! TT
  dtdxdudym=0.; dtdydudxm=0.; ! uT
  dtdxdudzm=0.; dtdzdudxm=0.;
  dtdydudzm=0.; dtdzdudym=0.;
  dtdxdvdym=0.; dtdydvdxm=0.; ! vT
  dtdxdvdzm=0.; dtdzdvdxm=0.;
  dtdydvdzm=0.; dtdzdvdym=0.;
  dtdxdwdym=0.; dtdydwdxm=0.; ! wT
  dtdxdwdzm=0.; dtdzdwdxm=0.;
  dtdydwdzm=0.; dtdzdwdym=0.;
  !
  vdtdxm=0.; vdtdym=0.; vdtdzm=0.;
  !
  dudx_dudtdxm=0.; dudx_dudtdym=0.; dudx_dudtdzm=0.;
  dudy_dvdtdxm=0.; dudy_dvdtdym=0.; dudy_dvdtdzm=0.;
  dudz_dwdtdxm=0.; dudz_dwdtdym=0.; dudz_dwdtdzm=0.;
  dtdx_dududym=0.; dtdx_dududzm=0.;
  dtdy_dudvdxm=0.; dtdy_dudvdzm=0.;
  dtdz_dudwdxm=0.; dtdz_dudwdym=0.;
  !
  dvdx_dudtdxm=0.; dvdx_dudtdym=0.; dvdx_dudtdzm=0.;
  dvdy_dvdtdxm=0.; dvdy_dvdtdym=0.; dvdy_dvdtdzm=0.;
  dvdz_dwdtdxm=0.; dvdz_dwdtdym=0.; dvdz_dwdtdzm=0.;
  dtdx_dudvdym=0.; dtdx_dudvdzm=0.;
  dtdy_dvdvdxm=0.; dtdy_dvdvdzm=0.;
  dtdz_dvdwdxm=0.; dtdz_dvdwdym=0.;
  !
  dtdx_dudtdxm=0.; dtdx_dudtdym=0.; dtdx_dudtdzm=0.;
  dtdy_dvdtdxm=0.; dtdy_dvdtdym=0.; dtdy_dvdtdzm=0.;
  dtdz_dwdtdxm=0.; dtdz_dwdtdym=0.; dtdz_dwdtdzm=0.;
  !
  vdudtdxm=0.; vdudtdym=0.; vdudtdzm=0.;
  vdvdtdxm=0.; vdvdtdym=0.; vdvdtdzm=0.;
  vdtdx2m=0.; vdtdy2m=0.; vdtdz2m=0.;
  !
  dpdtdxm=0.; dpdtdym=0.; dpdtdzm=0.;
  dpdtdxxm=0.; dpdtdxym=0.; dpdtdxzm=0.;
  dpdtdyxm=0.; dpdtdyym=0.; dpdtdyzm=0.;
  !
  dudtdxxm=0.; dudtdyym=0.; dudtdzzm=0.;
  dudtdxym=0.; dudtdxzm=0.; dudtdyzm=0.;
  !
  dudx_dtdxxxm=0.; dudx_dtdyxxm=0.; dudx_dtdzxxm=0.;
  dudy_dtdxyym=0.; dudy_dtdyyym=0.; dudy_dtdzyym=0.;
  dudz_dtdxzzm=0.; dudz_dtdyzzm=0.; dudz_dtdzzzm=0.;
  !
  dvdtdxxm=0.; dvdtdyym=0.; dvdtdzzm=0.;
  dvdtdxym=0.; dvdtdxzm=0.; dvdtdyzm=0.;
  !
  dvdx_dtdxxxm=0.; dvdx_dtdyxxm=0.; dvdx_dtdzxxm=0.;
  dvdy_dtdxyym=0.; dvdy_dtdyyym=0.; dvdy_dtdzyym=0.;
  dvdz_dtdxzzm=0.; dvdz_dtdyzzm=0.; dvdz_dtdzzzm=0.;
  !
  dtdxx2m=0.; dtdyy2m=0.; dtdzz2m=0.;
  dtdxy2m=0.; dtdxz2m=0.; dtdyz2m=0.;
  !
  ! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ! Ordre 1
  allocate(phims(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidyyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  ! Ordre 2
  allocate(uphims(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vphims(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wphims(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiphims(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phidpdxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phidpdyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phidpdzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidx2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dphidz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudphidxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdphidxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dwdphidxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudphidyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdphidyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dwdphidyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudphidzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdphidzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dwdphidzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(udtdxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(udtdyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(udtdzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wdtdxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wdtdyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wdtdzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  ! Ordre 3
  allocate(uphi2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vphi2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(wphi2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiuums(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phivvms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiwwms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiuvms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phiuwms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(phivwms(xst1:xen1,xst2:xen2,xst3:xen3))
  ! P2
  allocate(dtdxdtdyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! TT
  allocate(dtdxdtdzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdydtdzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdudyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! uT
  allocate(dtdydudxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdudzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdudxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdydudzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdudyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdvdyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! vT
  allocate(dtdydvdxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdvdzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdvdxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdydvdzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdvdyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdwdyms(xst1:xen1,xst2:xen2,xst3:xen3)) ! wT
  allocate(dtdydwdxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxdwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdwdxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdydwdzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzdwdyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(vdtdzms(xst1:xen1,xst2:xen2,xst3:xen3))
  ! xT
  allocate(dudtdxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdxyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdxzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudtdyzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudx_dtdxxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudx_dtdyxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudx_dtdzxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dtdxyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dtdyyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudy_dtdzyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dtdxzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dtdyzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dudz_dtdzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  ! yT
  allocate(dvdtdxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdxyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdxzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdtdyzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdx_dtdxxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdx_dtdyxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdx_dtdzxxms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dtdxyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dtdyyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdy_dtdzyyms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dtdxzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dtdyzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dvdz_dtdzzms(xst1:xen1,xst2:xen2,xst3:xen3))
  ! TT
  allocate(dtdxx2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdyy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdzz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxy2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdxz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  allocate(dtdyz2ms(xst1:xen1,xst2:xen2,xst3:xen3))
  !
  phims=0.; dphidyms=0.;
  dphidxms=0.; dphidzms=0.;
  dphidyyms=0.; dphidyyyms=0.;
  uphims=0.; vphims=0.; wphims=0.; phiphims=0.;
  phidpdxms=0.; phidpdyms=0.; phidpdzms=0.;
  dphidx2ms=0.; dphidy2ms=0.; dphidz2ms=0.;
  dudphidxms=0.; dvdphidxms=0.; dwdphidxms=0.;
  dudphidyms=0.; dvdphidyms=0.; dwdphidyms=0.;
  dudphidzms=0.; dvdphidzms=0.; dwdphidzms=0.;
  udtdxxms=0.; udtdyyms=0.; udtdzzms=0.;
  vdtdxxms=0.; vdtdyyms=0.; vdtdzzms=0.;
  wdtdxxms=0.; wdtdyyms=0.; wdtdzzms=0.;
  uphi2ms=0.; vphi2ms=0.; wphi2ms=0.;
  phiuums=0.; phivvms=0.; phiwwms=0.;
  phiuvms=0.; phiuwms=0.; phivwms=0.;
  !
  dtdxdtdyms=0.; dtdxdtdzms=0.; dtdydtdzms=0.; ! TT
  dtdxdudyms=0.; dtdydudxms=0.; ! uT
  dtdxdudzms=0.; dtdzdudxms=0.;
  dtdydudzms=0.; dtdzdudyms=0.;
  dtdxdvdyms=0.; dtdydvdxms=0.; ! vT
  dtdxdvdzms=0.; dtdzdvdxms=0.;
  dtdydvdzms=0.; dtdzdvdyms=0.;
  dtdxdwdyms=0.; dtdydwdxms=0.; ! wT
  dtdxdwdzms=0.; dtdzdwdxms=0.;
  dtdydwdzms=0.; dtdzdwdyms=0.;
  !
  vdtdxms=0.; vdtdyms=0.; vdtdzms=0.;
  !
  dudtdxxms=0.; dudtdyyms=0.; dudtdzzms=0.;
  dudtdxyms=0.; dudtdxzms=0.; dudtdyzms=0.;
  dudx_dtdxxxms=0.; dudx_dtdyxxms=0.; dudx_dtdzxxms=0.;
  dudy_dtdxyyms=0.; dudy_dtdyyyms=0.; dudy_dtdzyyms=0.;
  dudz_dtdxzzms=0.; dudz_dtdyzzms=0.; dudz_dtdzzms=0.;
  !
  dvdtdxxms=0.; dvdtdyyms=0.; dvdtdzzms=0.;
  dvdtdxyms=0.; dvdtdxzms=0.; dvdtdyzms=0.;
  dvdx_dtdxxxms=0.; dvdx_dtdyxxms=0.; dvdx_dtdzxxms=0.;
  dvdy_dtdxyyms=0.; dvdy_dtdyyyms=0.; dvdy_dtdzyyms=0.;
  dvdz_dtdxzzms=0.; dvdz_dtdyzzms=0.; dvdz_dtdzzms=0.;
  !
  dtdxx2ms=0.; dtdyy2ms=0.; dtdzz2ms=0.;
  dtdxy2ms=0.; dtdxz2ms=0.; dtdyz2ms=0.;
  !
endif

! Pression
allocate(presm(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(pres2m(xst1:xen1,xst2:xen2,xst3:xen3))
!
presm=0.; pres2m=0.;
!


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! Pression
allocate(presms(xst1:xen1,xst2:xen2,xst3:xen3))
allocate(pres2ms(xst1:xen1,xst2:xen2,xst3:xen3))
!
presms=0.; pres2ms=0.;
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

end subroutine allocate_user_stats

subroutine update_user_stats_non_phi(ux,uy,uz,dpdx,dpdy,dpdz,&
						dudx,dudy,dudz,&
						dvdx,dvdy,dvdz,&
						dwdx,dwdy,dwdz,&
						dudxx, dudyy, dudzz, dudxy, dudxz, dudyz,&
						dvdxx, dvdyy, dvdzz, dvdxy, dvdxz, dvdyz,&
						dwdxx, dwdyy, dwdzz, dwdxy, dwdxz, dwdyz,&
						     pres,pp3,&
                                                     phG,ph1,ph2,ph3,ph4)

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!use decomp_2d, only : xsize, nrank, DECOMP_INFO
use decomp_2d, only : xsize, DECOMP_INFO
use decomp_2d_mpi, only : nrank                                                     
use param, only : itime
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

implicit none

TYPE(DECOMP_INFO), intent(in) :: phG,ph1,ph2,ph3,ph4
real(mytype), dimension(xsize(1),xsize(2),xsize(3)), intent(in) :: ux,uy,uz,&
						dpdx, dpdy, dpdz,&
						dudx, dudy, dudz,&
						dvdx, dvdy, dvdz,&
						dwdx, dwdy, dwdz,&
						dudxx, dudyy, dudzz, dudxy, dudxz, dudyz,&
						dvdxx, dvdyy, dvdzz, dvdxy, dvdxz, dvdyz,&
						dwdxx, dwdyy, dwdzz, dwdxy, dwdxz, dwdyz,&
						pres
real(mytype), dimension(:,:,:), intent(in) :: pp3

! Local variable
integer :: coeff

coeff=itime-beg_stat
if (coeff.le.0) then
  if (nrank.eq.0) print*,'ERROR : cannot start to compute statistics at ',itime,'. Wait till ',beg_stat
  return
endif

! Ojo en mecclsut: ~/CHT/PruCe/Pru1b/ ya habia realizado esto con exito. simplemente no usar el fine_interpol para hacer el promediado en todo el dominio (si se desea hacer en planos pues sí usarlo!)
!um = (real(coeff-1, kind=mytype)*um + (ux)) / real(coeff, kind=mytype)
um = (real(coeff-1, kind=mytype)*um + ux) / real(coeff, kind=mytype)
vm = (real(coeff-1, kind=mytype)*vm + uy) / real(coeff, kind=mytype)
wm = (real(coeff-1, kind=mytype)*wm + uz) / real(coeff, kind=mytype)

dpdxm = (real(coeff-1, kind=mytype)*dpdxm + dpdx) / real(coeff, kind=mytype)
dpdym = (real(coeff-1, kind=mytype)*dpdym + dpdy) / real(coeff, kind=mytype)
dpdzm = (real(coeff-1, kind=mytype)*dpdzm + dpdz) / real(coeff, kind=mytype)

dudym = (real(coeff-1, kind=mytype)*dudym + dudy) / real(coeff, kind=mytype)
dvdym = (real(coeff-1, kind=mytype)*dvdym + dvdy) / real(coeff, kind=mytype)
dwdym = (real(coeff-1, kind=mytype)*dwdym + dwdy) / real(coeff, kind=mytype)
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
dudxm = (real(coeff-1, kind=mytype)*dudxm + dudx) / real(coeff, kind=mytype)
dvdxm = (real(coeff-1, kind=mytype)*dvdxm + dvdx) / real(coeff, kind=mytype)
dwdxm = (real(coeff-1, kind=mytype)*dwdxm + dwdx) / real(coeff, kind=mytype)

dudzm = (real(coeff-1, kind=mytype)*dudzm + dudz) / real(coeff, kind=mytype)
dvdzm = (real(coeff-1, kind=mytype)*dvdzm + dvdz) / real(coeff, kind=mytype)
dwdzm = (real(coeff-1, kind=mytype)*dwdzm + dwdz) / real(coeff, kind=mytype)

!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

dudyym = (real(coeff-1, kind=mytype)*dudyym + dudyy) / real(coeff, kind=mytype)
dvdyym = (real(coeff-1, kind=mytype)*dvdyym + dvdyy) / real(coeff, kind=mytype)
dwdyym = (real(coeff-1, kind=mytype)*dwdyym + dwdyy) / real(coeff, kind=mytype)

uum = (real(coeff-1, kind=mytype)*uum + ux*ux) / real(coeff, kind=mytype)
vvm = (real(coeff-1, kind=mytype)*vvm + uy*uy) / real(coeff, kind=mytype)
wwm = (real(coeff-1, kind=mytype)*wwm + uz*uz) / real(coeff, kind=mytype)

uvm = (real(coeff-1, kind=mytype)*uvm + ux*uy) / real(coeff, kind=mytype)
uwm = (real(coeff-1, kind=mytype)*uwm + ux*uz) / real(coeff, kind=mytype)
vwm = (real(coeff-1, kind=mytype)*vwm + uy*uz) / real(coeff, kind=mytype)

udpdxm = (real(coeff-1, kind=mytype)*udpdxm + ux*dpdx) / real(coeff, kind=mytype)
udpdym = (real(coeff-1, kind=mytype)*udpdym + ux*dpdy) / real(coeff, kind=mytype)
udpdzm = (real(coeff-1, kind=mytype)*udpdzm + ux*dpdz) / real(coeff, kind=mytype)

vdpdxm = (real(coeff-1, kind=mytype)*vdpdxm + uy*dpdx) / real(coeff, kind=mytype)
vdpdym = (real(coeff-1, kind=mytype)*vdpdym + uy*dpdy) / real(coeff, kind=mytype)
vdpdzm = (real(coeff-1, kind=mytype)*vdpdzm + uy*dpdz) / real(coeff, kind=mytype)

wdpdxm = (real(coeff-1, kind=mytype)*wdpdxm + uz*dpdx) / real(coeff, kind=mytype)
wdpdym = (real(coeff-1, kind=mytype)*wdpdym + uz*dpdy) / real(coeff, kind=mytype)
wdpdzm = (real(coeff-1, kind=mytype)*wdpdzm + uz*dpdz) / real(coeff, kind=mytype)

uuum = (real(coeff-1, kind=mytype)*uuum + ux*ux*ux) / real(coeff, kind=mytype)
uvvm = (real(coeff-1, kind=mytype)*uvvm + (ux*uy*uy)) / real(coeff, kind=mytype)
uwwm = (real(coeff-1, kind=mytype)*uwwm + (ux*uz*uz)) / real(coeff, kind=mytype)

vuum = (real(coeff-1, kind=mytype)*vuum + (uy*ux*ux)) / real(coeff, kind=mytype)
vvvm = (real(coeff-1, kind=mytype)*vvvm + (uy*uy*uy)) / real(coeff, kind=mytype)
vwwm = (real(coeff-1, kind=mytype)*vwwm + (uy*uz*uz)) / real(coeff, kind=mytype)

wuum = (real(coeff-1, kind=mytype)*wuum + (uz*ux*ux)) / real(coeff, kind=mytype)
wvvm = (real(coeff-1, kind=mytype)*wvvm + (uz*uy*uy)) / real(coeff, kind=mytype)
wwwm = (real(coeff-1, kind=mytype)*wwwm + (uz*uz*uz)) / real(coeff, kind=mytype)

uvwm = (real(coeff-1, kind=mytype)*uvwm + (ux*uy*uz)) / real(coeff, kind=mytype)

dudx2m = (real(coeff-1, kind=mytype)*dudx2m + (dudx*dudx)) / real(coeff, kind=mytype)
dudy2m = (real(coeff-1, kind=mytype)*dudy2m + (dudy*dudy)) / real(coeff, kind=mytype)
dudz2m = (real(coeff-1, kind=mytype)*dudz2m + (dudz*dudz)) / real(coeff, kind=mytype)

dvdx2m = (real(coeff-1, kind=mytype)*dvdx2m + (dvdx*dvdx)) / real(coeff, kind=mytype)
dvdy2m = (real(coeff-1, kind=mytype)*dvdy2m + (dvdy*dvdy)) / real(coeff, kind=mytype)
dvdz2m = (real(coeff-1, kind=mytype)*dvdz2m + (dvdz*dvdz)) / real(coeff, kind=mytype)

dwdx2m = (real(coeff-1, kind=mytype)*dwdx2m + (dwdx*dwdx)) / real(coeff, kind=mytype)
dwdy2m = (real(coeff-1, kind=mytype)*dwdy2m + (dwdy*dwdy)) / real(coeff, kind=mytype)
dwdz2m = (real(coeff-1, kind=mytype)*dwdz2m + (dwdz*dwdz)) / real(coeff, kind=mytype)

dudvdxm= (real(coeff-1, kind=mytype)*dudvdxm+ real(1, kind=mytype)*(dudx*dvdx)) / real(coeff, kind=mytype)
dudwdxm= (real(coeff-1, kind=mytype)*dudwdxm+ real(1, kind=mytype)*(dudx*dwdx)) / real(coeff, kind=mytype)
dvdwdxm= (real(coeff-1, kind=mytype)*dvdwdxm+ real(1, kind=mytype)*(dvdx*dwdx)) / real(coeff, kind=mytype)

dudvdym= (real(coeff-1, kind=mytype)*dudvdym+ real(1, kind=mytype)*(dudy*dvdy)) / real(coeff, kind=mytype)
dudwdym= (real(coeff-1, kind=mytype)*dudwdym+ real(1, kind=mytype)*(dudy*dwdy)) / real(coeff, kind=mytype)
dvdwdym= (real(coeff-1, kind=mytype)*dvdwdym+ real(1, kind=mytype)*(dvdy*dwdy)) / real(coeff, kind=mytype)

dudvdzm= (real(coeff-1, kind=mytype)*dudvdzm+ real(1, kind=mytype)*(dudz*dvdz)) / real(coeff, kind=mytype)
dudwdzm= (real(coeff-1, kind=mytype)*dudwdzm+ real(1, kind=mytype)*(dudz*dwdz)) / real(coeff, kind=mytype)
dvdwdzm= (real(coeff-1, kind=mytype)*dvdwdzm+ real(1, kind=mytype)*(dvdz*dwdz)) / real(coeff, kind=mytype)

dudxdudym = (real(coeff-1, kind=mytype)*dudxdudym + (dudx*dudy)) / real(coeff, kind=mytype) ! uu
dudxdudzm = (real(coeff-1, kind=mytype)*dudxdudzm + (dudx*dudz)) / real(coeff, kind=mytype)
dudydudzm = (real(coeff-1, kind=mytype)*dudydudzm + (dudy*dudz)) / real(coeff, kind=mytype)
dvdxdvdym = (real(coeff-1, kind=mytype)*dvdxdvdym + (dvdx*dvdy)) / real(coeff, kind=mytype) ! vv
dvdxdvdzm = (real(coeff-1, kind=mytype)*dvdxdvdzm + (dvdx*dvdz)) / real(coeff, kind=mytype)
dvdydvdzm = (real(coeff-1, kind=mytype)*dvdydvdzm + (dvdy*dvdz)) / real(coeff, kind=mytype)
dwdxdwdym = (real(coeff-1, kind=mytype)*dwdxdwdym + (dwdx*dwdy)) / real(coeff, kind=mytype) ! ww
dwdxdwdzm = (real(coeff-1, kind=mytype)*dwdxdwdzm + (dwdx*dwdz)) / real(coeff, kind=mytype)
dwdydwdzm = (real(coeff-1, kind=mytype)*dwdydwdzm + (dwdy*dwdz)) / real(coeff, kind=mytype)

dudxdvdym = (real(coeff-1, kind=mytype)*dudxdvdym + (dudx*dvdy)) / real(coeff, kind=mytype) ! uv
dudydvdxm = (real(coeff-1, kind=mytype)*dudydvdxm + (dudy*dvdx)) / real(coeff, kind=mytype)
dudxdvdzm = (real(coeff-1, kind=mytype)*dudxdvdzm + (dudx*dvdz)) / real(coeff, kind=mytype)
dudzdvdxm = (real(coeff-1, kind=mytype)*dudzdvdxm + (dudz*dvdx)) / real(coeff, kind=mytype)
dudydvdzm = (real(coeff-1, kind=mytype)*dudydvdzm + (dudy*dvdz)) / real(coeff, kind=mytype)
dudzdvdym = (real(coeff-1, kind=mytype)*dudzdvdym + (dudz*dvdy)) / real(coeff, kind=mytype)

dudxdwdym = (real(coeff-1, kind=mytype)*dudxdwdym + (dudx*dwdy)) / real(coeff, kind=mytype) ! uw
dudydwdxm = (real(coeff-1, kind=mytype)*dudydwdxm + (dudy*dwdx)) / real(coeff, kind=mytype)
dudxdwdzm = (real(coeff-1, kind=mytype)*dudxdwdzm + (dudx*dwdz)) / real(coeff, kind=mytype)
dudzdwdxm = (real(coeff-1, kind=mytype)*dudzdwdxm + (dudz*dwdx)) / real(coeff, kind=mytype)
dudydwdzm = (real(coeff-1, kind=mytype)*dudydwdzm + (dudy*dwdz)) / real(coeff, kind=mytype)
dudzdwdym = (real(coeff-1, kind=mytype)*dudzdwdym + (dudz*dwdy)) / real(coeff, kind=mytype)

dvdxdwdym = (real(coeff-1, kind=mytype)*dvdxdwdym + (dvdx*dwdy)) / real(coeff, kind=mytype) ! vw
dvdydwdxm = (real(coeff-1, kind=mytype)*dvdydwdxm + (dvdy*dwdx)) / real(coeff, kind=mytype)
dvdxdwdzm = (real(coeff-1, kind=mytype)*dvdxdwdzm + (dvdx*dwdz)) / real(coeff, kind=mytype)
dvdzdwdxm = (real(coeff-1, kind=mytype)*dvdzdwdxm + (dvdz*dwdx)) / real(coeff, kind=mytype)
dvdydwdzm = (real(coeff-1, kind=mytype)*dvdydwdzm + (dvdy*dwdz)) / real(coeff, kind=mytype)
dvdzdwdym = (real(coeff-1, kind=mytype)*dvdzdwdym + (dvdz*dwdy)) / real(coeff, kind=mytype)

vdudxm = (real(coeff-1, kind=mytype)*vdudxm + (uy*dudx)) / real(coeff, kind=mytype)
vdudym = (real(coeff-1, kind=mytype)*vdudym + (uy*dudy)) / real(coeff, kind=mytype)
vdudzm = (real(coeff-1, kind=mytype)*vdudzm + (uy*dudz)) / real(coeff, kind=mytype)
vdvdxm = (real(coeff-1, kind=mytype)*vdvdxm + (uy*dvdx)) / real(coeff, kind=mytype)
vdvdym = (real(coeff-1, kind=mytype)*vdvdym + (uy*dvdy)) / real(coeff, kind=mytype)
vdvdzm = (real(coeff-1, kind=mytype)*vdvdzm + (uy*dvdz)) / real(coeff, kind=mytype)
vdwdxm = (real(coeff-1, kind=mytype)*vdwdxm + (uy*dwdx)) / real(coeff, kind=mytype)
vdwdym = (real(coeff-1, kind=mytype)*vdwdym + (uy*dwdy)) / real(coeff, kind=mytype)
vdwdzm = (real(coeff-1, kind=mytype)*vdwdzm + (uy*dwdz)) / real(coeff, kind=mytype)

dudx_dududxm = (real(coeff-1, kind=mytype)*dudx_dududxm + (dudx*dudx*dudx)) / real(coeff, kind=mytype) ! dudx_eps11
dudx_dududym = (real(coeff-1, kind=mytype)*dudx_dududym + (dudx*dudy*dudy)) / real(coeff, kind=mytype)
dudx_dududzm = (real(coeff-1, kind=mytype)*dudx_dududzm + (dudx*dudz*dudz)) / real(coeff, kind=mytype)
dudy_dudvdxm = (real(coeff-1, kind=mytype)*dudy_dudvdxm + (dudy*dudx*dvdx)) / real(coeff, kind=mytype) ! dudy_eps12
dudy_dudvdym = (real(coeff-1, kind=mytype)*dudy_dudvdym + (dudy*dudy*dvdy)) / real(coeff, kind=mytype)
dudy_dudvdzm = (real(coeff-1, kind=mytype)*dudy_dudvdzm + (dudy*dudz*dvdz)) / real(coeff, kind=mytype)
dudz_dudwdxm = (real(coeff-1, kind=mytype)*dudz_dudwdxm + (dudz*dudx*dwdx)) / real(coeff, kind=mytype) ! dudz_eps13
dudz_dudwdym = (real(coeff-1, kind=mytype)*dudz_dudwdym + (dudz*dudy*dwdy)) / real(coeff, kind=mytype)
dudz_dudwdzm = (real(coeff-1, kind=mytype)*dudz_dudwdzm + (dudz*dudz*dwdz)) / real(coeff, kind=mytype)

dvdx_dvdudxm = (real(coeff-1, kind=mytype)*dvdx_dvdudxm + (dvdx*dvdx*dudx)) / real(coeff, kind=mytype) ! dvdx_eps12
dvdx_dvdudym = (real(coeff-1, kind=mytype)*dvdx_dvdudym + (dvdx*dvdy*dudy)) / real(coeff, kind=mytype)
dvdx_dvdudzm = (real(coeff-1, kind=mytype)*dvdx_dvdudzm + (dvdx*dvdz*dudz)) / real(coeff, kind=mytype)
dvdy_dvdvdxm = (real(coeff-1, kind=mytype)*dvdy_dvdvdxm + (dvdy*dvdx*dvdx)) / real(coeff, kind=mytype) ! dvdy_eps22
dvdy_dvdvdym = (real(coeff-1, kind=mytype)*dvdy_dvdvdym + (dvdy*dvdy*dvdy)) / real(coeff, kind=mytype)
dvdy_dvdvdzm = (real(coeff-1, kind=mytype)*dvdy_dvdvdzm + (dvdy*dvdz*dvdz)) / real(coeff, kind=mytype)
dvdz_dvdwdxm = (real(coeff-1, kind=mytype)*dvdz_dvdwdxm + (dvdz*dvdx*dwdx)) / real(coeff, kind=mytype) ! dvdz_eps23
dvdz_dvdwdym = (real(coeff-1, kind=mytype)*dvdz_dvdwdym + (dvdz*dvdy*dwdy)) / real(coeff, kind=mytype)
dvdz_dvdwdzm = (real(coeff-1, kind=mytype)*dvdz_dvdwdzm + (dvdz*dvdz*dwdz)) / real(coeff, kind=mytype)

dwdx_dwdudxm = (real(coeff-1, kind=mytype)*dwdx_dwdudxm + (dwdx*dwdx*dudx)) / real(coeff, kind=mytype) ! dwdx_eps13
dwdx_dwdudym = (real(coeff-1, kind=mytype)*dwdx_dwdudym + (dwdx*dwdy*dudy)) / real(coeff, kind=mytype)
dwdx_dwdudzm = (real(coeff-1, kind=mytype)*dwdx_dwdudzm + (dwdx*dwdz*dudz)) / real(coeff, kind=mytype)
dwdy_dwdvdxm = (real(coeff-1, kind=mytype)*dwdy_dwdvdxm + (dwdy*dwdx*dvdx)) / real(coeff, kind=mytype) ! dwdy_eps23
dwdy_dwdvdym = (real(coeff-1, kind=mytype)*dwdy_dwdvdym + (dwdy*dwdy*dvdy)) / real(coeff, kind=mytype)
dwdy_dwdvdzm = (real(coeff-1, kind=mytype)*dwdy_dwdvdzm + (dwdy*dwdz*dvdz)) / real(coeff, kind=mytype)
dwdz_dwdwdxm = (real(coeff-1, kind=mytype)*dwdz_dwdwdxm + (dwdz*dwdx*dwdx)) / real(coeff, kind=mytype) ! dwdz_eps33
dwdz_dwdwdym = (real(coeff-1, kind=mytype)*dwdz_dwdwdym + (dwdz*dwdy*dwdy)) / real(coeff, kind=mytype)
dwdz_dwdwdzm = (real(coeff-1, kind=mytype)*dwdz_dwdwdzm + (dwdz*dwdz*dwdz)) / real(coeff, kind=mytype)

dudx_dudvdxm = (real(coeff-1, kind=mytype)*dudx_dudvdxm + (dudx*dudx*dvdx)) / real(coeff, kind=mytype) ! dudx_eps21
dudx_dudvdym = (real(coeff-1, kind=mytype)*dudx_dudvdym + (dudx*dudy*dvdy)) / real(coeff, kind=mytype)
dudx_dudvdzm = (real(coeff-1, kind=mytype)*dudx_dudvdzm + (dudx*dudz*dvdz)) / real(coeff, kind=mytype)
dudy_dvdvdxm = (real(coeff-1, kind=mytype)*dudy_dvdvdxm + (dudy*dvdx*dvdx)) / real(coeff, kind=mytype) ! dudy_eps22
dudy_dvdvdym = (real(coeff-1, kind=mytype)*dudy_dvdvdym + (dudy*dvdy*dvdy)) / real(coeff, kind=mytype)
dudy_dvdvdzm = (real(coeff-1, kind=mytype)*dudy_dvdvdzm + (dudy*dvdz*dvdz)) / real(coeff, kind=mytype)
dudz_dvdwdxm = (real(coeff-1, kind=mytype)*dudz_dvdwdxm + (dudz*dvdx*dwdx)) / real(coeff, kind=mytype) ! dudz_eps23
dudz_dvdwdym = (real(coeff-1, kind=mytype)*dudz_dvdwdym + (dudz*dvdy*dwdy)) / real(coeff, kind=mytype)
dudz_dvdwdzm = (real(coeff-1, kind=mytype)*dudz_dvdwdzm + (dudz*dvdz*dwdz)) / real(coeff, kind=mytype)
dvdx_dududxm = (real(coeff-1, kind=mytype)*dvdx_dududxm + (dvdx*dudx*dudx)) / real(coeff, kind=mytype) ! dvdx_eps11
dvdx_dududym = (real(coeff-1, kind=mytype)*dvdx_dududym + (dvdx*dudy*dudy)) / real(coeff, kind=mytype)
dvdx_dududzm = (real(coeff-1, kind=mytype)*dvdx_dududzm + (dvdx*dudz*dudz)) / real(coeff, kind=mytype)
dvdy_dudvdxm = (real(coeff-1, kind=mytype)*dvdy_dudvdxm + (dvdy*dudx*dvdx)) / real(coeff, kind=mytype) ! dvdy_eps12
dvdy_dudvdym = (real(coeff-1, kind=mytype)*dvdy_dudvdym + (dvdy*dudy*dvdy)) / real(coeff, kind=mytype)
dvdy_dudvdzm = (real(coeff-1, kind=mytype)*dvdy_dudvdzm + (dvdy*dudz*dvdz)) / real(coeff, kind=mytype)
dvdz_dudwdxm = (real(coeff-1, kind=mytype)*dvdz_dudwdxm + (dvdz*dudx*dwdx)) / real(coeff, kind=mytype) ! dvdz_eps13
dvdz_dudwdym = (real(coeff-1, kind=mytype)*dvdz_dudwdym + (dvdz*dudy*dwdy)) / real(coeff, kind=mytype)
dvdz_dudwdzm = (real(coeff-1, kind=mytype)*dvdz_dudwdzm + (dvdz*dudz*dwdz)) / real(coeff, kind=mytype)

vdudx2m = (real(coeff-1, kind=mytype)*vdudx2m + (uy*dudx*dudx)) / real(coeff, kind=mytype) ! EpsXX
vdudy2m = (real(coeff-1, kind=mytype)*vdudy2m + (uy*dudy*dudy)) / real(coeff, kind=mytype)
vdudz2m = (real(coeff-1, kind=mytype)*vdudz2m + (uy*dudz*dudz)) / real(coeff, kind=mytype)
vdvdx2m = (real(coeff-1, kind=mytype)*vdvdx2m + (uy*dvdx*dvdx)) / real(coeff, kind=mytype) ! EpsYY
vdvdy2m = (real(coeff-1, kind=mytype)*vdvdy2m + (uy*dvdy*dvdy)) / real(coeff, kind=mytype)
vdvdz2m = (real(coeff-1, kind=mytype)*vdvdz2m + (uy*dvdz*dvdz)) / real(coeff, kind=mytype)
vdwdx2m = (real(coeff-1, kind=mytype)*vdwdx2m + (uy*dwdx*dwdx)) / real(coeff, kind=mytype) ! EpsZZ
vdwdy2m = (real(coeff-1, kind=mytype)*vdwdy2m + (uy*dwdy*dwdy)) / real(coeff, kind=mytype)
vdwdz2m = (real(coeff-1, kind=mytype)*vdwdz2m + (uy*dwdz*dwdz)) / real(coeff, kind=mytype)
vdudvdxm = (real(coeff-1, kind=mytype)*vdudvdxm + (uy*dudx*dvdx)) / real(coeff, kind=mytype) ! EpsXY
vdudvdym = (real(coeff-1, kind=mytype)*vdudvdym + (uy*dudy*dvdy)) / real(coeff, kind=mytype)
vdudvdzm = (real(coeff-1, kind=mytype)*vdudvdzm + (uy*dudz*dvdz)) / real(coeff, kind=mytype)

dpdvdxm = (real(coeff-1, kind=mytype)*dpdvdxm + (dpdx*dvdx)) / real(coeff, kind=mytype) ! EpsYY
dpdvdym = (real(coeff-1, kind=mytype)*dpdvdym + (dpdy*dvdy)) / real(coeff, kind=mytype)
dpdvdzm = (real(coeff-1, kind=mytype)*dpdvdzm + (dpdz*dvdz)) / real(coeff, kind=mytype)
dpdudxm = (real(coeff-1, kind=mytype)*dpdudxm + (dpdx*dudx)) / real(coeff, kind=mytype) ! EpsXY
dpdudym = (real(coeff-1, kind=mytype)*dpdudym + (dpdy*dudy)) / real(coeff, kind=mytype)
dpdudzm = (real(coeff-1, kind=mytype)*dpdudzm + (dpdz*dudz)) / real(coeff, kind=mytype)

dpdxdudyxm = (real(coeff-1, kind=mytype)*dpdxdudyxm + (dpdx*dudxy)) / real(coeff, kind=mytype)
dpdydudyym = (real(coeff-1, kind=mytype)*dpdydudyym + (dpdy*dudyy)) / real(coeff, kind=mytype)
dpdzdudyzm = (real(coeff-1, kind=mytype)*dpdzdudyzm + (dpdz*dudyz)) / real(coeff, kind=mytype)
dpdxdvdxxm = (real(coeff-1, kind=mytype)*dpdxdvdxxm + (dpdx*dvdxx)) / real(coeff, kind=mytype)
dpdydvdxym = (real(coeff-1, kind=mytype)*dpdydvdxym + (dpdy*dvdxy)) / real(coeff, kind=mytype)
dpdzdvdxzm = (real(coeff-1, kind=mytype)*dpdzdvdxzm + (dpdz*dvdxz)) / real(coeff, kind=mytype)

dudxx2m = (real(coeff-1, kind=mytype)*dudxx2m + (dudxx*dudxx)) / real(coeff, kind=mytype) ! uu
dudyy2m = (real(coeff-1, kind=mytype)*dudyy2m + (dudyy*dudyy)) / real(coeff, kind=mytype)
dudzz2m = (real(coeff-1, kind=mytype)*dudzz2m + (dudzz*dudzz)) / real(coeff, kind=mytype)
dudxy2m = (real(coeff-1, kind=mytype)*dudxy2m + (dudxy*dudxy)) / real(coeff, kind=mytype)
dudxz2m = (real(coeff-1, kind=mytype)*dudxz2m + (dudxz*dudxz)) / real(coeff, kind=mytype)
dudyz2m = (real(coeff-1, kind=mytype)*dudyz2m + (dudyz*dudyz)) / real(coeff, kind=mytype)

dvdxx2m = (real(coeff-1, kind=mytype)*dvdxx2m + (dvdxx*dvdxx)) / real(coeff, kind=mytype) ! vv
dvdyy2m = (real(coeff-1, kind=mytype)*dvdyy2m + (dvdyy*dvdyy)) / real(coeff, kind=mytype)
dvdzz2m = (real(coeff-1, kind=mytype)*dvdzz2m + (dvdzz*dvdzz)) / real(coeff, kind=mytype)
dvdxy2m = (real(coeff-1, kind=mytype)*dvdxy2m + (dvdxy*dvdxy)) / real(coeff, kind=mytype)
dvdxz2m = (real(coeff-1, kind=mytype)*dvdxz2m + (dvdxz*dvdxz)) / real(coeff, kind=mytype)
dvdyz2m = (real(coeff-1, kind=mytype)*dvdyz2m + (dvdyz*dvdyz)) / real(coeff, kind=mytype)

dwdxx2m = (real(coeff-1, kind=mytype)*dwdxx2m + (dwdxx*dwdxx)) / real(coeff, kind=mytype) ! ww
dwdyy2m = (real(coeff-1, kind=mytype)*dwdyy2m + (dwdyy*dwdyy)) / real(coeff, kind=mytype)
dwdzz2m = (real(coeff-1, kind=mytype)*dwdzz2m + (dwdzz*dwdzz)) / real(coeff, kind=mytype)
dwdxy2m = (real(coeff-1, kind=mytype)*dwdxy2m + (dwdxy*dwdxy)) / real(coeff, kind=mytype)
dwdxz2m = (real(coeff-1, kind=mytype)*dwdxz2m + (dwdxz*dwdxz)) / real(coeff, kind=mytype)
dwdyz2m = (real(coeff-1, kind=mytype)*dwdyz2m + (dwdyz*dwdyz)) / real(coeff, kind=mytype)

dudvdxx2m = (real(coeff-1, kind=mytype)*dudvdxx2m + (dudxx*dvdxx)) / real(coeff, kind=mytype) ! uv
dudvdyy2m = (real(coeff-1, kind=mytype)*dudvdyy2m + (dudyy*dvdyy)) / real(coeff, kind=mytype)
dudvdzz2m = (real(coeff-1, kind=mytype)*dudvdzz2m + (dudzz*dvdzz)) / real(coeff, kind=mytype)
dudvdxy2m = (real(coeff-1, kind=mytype)*dudvdxy2m + (dudxy*dvdxy)) / real(coeff, kind=mytype)
dudvdxz2m = (real(coeff-1, kind=mytype)*dudvdxz2m + (dudxz*dvdxz)) / real(coeff, kind=mytype)
dudvdyz2m = (real(coeff-1, kind=mytype)*dudvdyz2m + (dudyz*dvdyz)) / real(coeff, kind=mytype)

presm  = (real(coeff-1, kind=mytype)*presm  + real(1, kind=mytype)*(pres)) / real(coeff, kind=mytype)
pres2m = (real(coeff-1, kind=mytype)*pres2m + (pres*pres)) / real(coeff, kind=mytype)

end subroutine update_user_stats_non_phi

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
subroutine update_user_stats_spat(ux,uy,uz,dpdx,dpdy,dpdz,&
						dudt,dvdt,dwdt,&
						dudx,dudy,dudz,&
						dvdx,dvdy,dvdz,&
						dwdx,dwdy,dwdz,&
						dudxx, dudyy, dudzz, dudxy, dudxz, dudyz,&
						dvdxx, dvdyy, dvdzz, dvdxy, dvdxz, dvdyz,&
						dwdxx, dwdyy, dwdzz, dwdxy, dwdxz, dwdyz,&
						     pres,pp3,&
                                                     phG,ph1,ph2,ph3,ph4)

use decomp_2d, only : xsize, DECOMP_INFO
use decomp_2d_mpi, only : nrank    
use param, only : itime

implicit none

TYPE(DECOMP_INFO), intent(in) :: phG,ph1,ph2,ph3,ph4
real(mytype), dimension(xsize(1),xsize(2),xsize(3)), intent(in) :: ux,uy,uz,&
						dpdx, dpdy, dpdz,&
						dudt, dvdt, dwdt,&
						dudx, dudy, dudz,&
						dvdx, dvdy, dvdz,&
						dwdx, dwdy, dwdz,&
						dudxx, dudyy, dudzz, dudxy, dudxz, dudyz,&
						dvdxx, dvdyy, dvdzz, dvdxy, dvdxz, dvdyz,&
						dwdxx, dwdyy, dwdzz, dwdxy, dwdxz, dwdyz,&
						pres
real(mytype), dimension(ph3%zst(1):ph3%zen(1),ph3%zst(2):ph3%zen(2),ph3%zst(3):ph3%zen(3)), intent(in) :: pp3

! Local variable
integer :: coeff

!coeff=itime-beg_stat
!if (coeff.le.0) then
!  if (nrank.eq.0) print*,'ERROR : cannot start to compute statistics at ',itime,'. Wait till ',beg_stat
!  return
!endif

! Ojo en mecclsut: ~/CHT/PruCe/Pru1b/ ya habia realizado esto con exito. simplemente no usar el fine_interpol para hacer el promediado en todo el dominio (si se desea hacer en planos pues sí usarlo!)
!um = (real(coeff-1, kind=mytype)*um + (ux)) / real(coeff, kind=mytype)
ums = ux
vms = uy
wms = uz

dpdxms = dpdx
dpdyms = dpdy
dpdzms = dpdz

dudyms = dudy
dvdyms = dvdy
dwdyms = dwdy

!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
dudtms = dudt
dvdtms = dvdt
dwdtms = dwdt

dudxms = dudx
dvdxms = dvdx
dwdxms = dwdx

dudzms = dudz
dvdzms = dvdz
dwdzms = dwdz
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

dudyyms = dudyy
dvdyyms = dvdyy
dwdyyms = dwdyy

uums = ux*ux
vvms = uy*uy
wwms = uz*uz

uvms = ux*uy
uwms = ux*uz
vwms = uy*uz

udpdxms = ux*dpdx
udpdyms = ux*dpdy
udpdzms = ux*dpdz

vdpdxms =  uy*dpdx
vdpdyms =  uy*dpdy
vdpdzms =  uy*dpdz

wdpdxms =  uz*dpdx
wdpdyms =  uz*dpdy
wdpdzms =  uz*dpdz

uuums =  ux*ux*ux
uvvms =  (ux*uy*uy)
uwwms =  (ux*uz*uz)

vuums =  (uy*ux*ux)
vvvms =  (uy*uy*uy)
vwwms =  (uy*uz*uz)

wuums =  (uz*ux*ux)
wvvms =  (uz*uy*uy)
wwwms =  (uz*uz*uz)

uvwms =  (ux*uy*uz)

dudx2ms =  (dudx*dudx)
dudy2ms =  (dudy*dudy)
dudz2ms =  (dudz*dudz)

dvdx2ms =  (dvdx*dvdx)
dvdy2ms =  (dvdy*dvdy)
dvdz2ms =  (dvdz*dvdz)

dwdx2ms =  (dwdx*dwdx)
dwdy2ms =  (dwdy*dwdy)
dwdz2ms =  (dwdz*dwdz)

dudvdxms=  (dudx*dvdx)
dudwdxms=  (dudx*dwdx)  
dvdwdxms=  (dvdx*dwdx)

dudvdyms=  (dudy*dvdy)
dudwdyms=  (dudy*dwdy)
dvdwdyms=  (dvdy*dwdy)

dudvdzms=  (dudz*dvdz)
dudwdzms=  (dudz*dwdz)
dvdwdzms=  (dvdz*dwdz)

dudxdudyms =  (dudx*dudy)   ! uu
dudxdudzms =  (dudx*dudz)  
dudydudzms =  (dudy*dudz)  
dvdxdvdyms =  (dvdx*dvdy)   ! vv
dvdxdvdzms =  (dvdx*dvdz)  
dvdydvdzms =  (dvdy*dvdz)  
dwdxdwdyms =  (dwdx*dwdy)   ! ww
dwdxdwdzms =  (dwdx*dwdz)  
dwdydwdzms =  (dwdy*dwdz)  

dudxdvdyms =  (dudx*dvdy)   ! uv
dudydvdxms =  (dudy*dvdx)  
dudxdvdzms =  (dudx*dvdz)
dudzdvdxms =  (dudz*dvdx)
dudydvdzms =  (dudy*dvdz)
dudzdvdyms =  (dudz*dvdy)

dudxdwdyms =  (dudx*dwdy)   ! uw
dudydwdxms =  (dudy*dwdx)
dudxdwdzms =  (dudx*dwdz)
dudzdwdxms =  (dudz*dwdx)
dudydwdzms =  (dudy*dwdz)
dudzdwdyms =  (dudz*dwdy)

dvdxdwdyms =  (dvdx*dwdy)   ! vw
dvdydwdxms =  (dvdy*dwdx)
dvdxdwdzms =  (dvdx*dwdz)
dvdzdwdxms =  (dvdz*dwdx)
dvdydwdzms =  (dvdy*dwdz)
dvdzdwdyms =  (dvdz*dwdy)

vdudxms =  (uy*dudx)
vdudyms =  (uy*dudy)
vdudzms =  (uy*dudz)
vdvdxms =  (uy*dvdx)
vdvdyms =  (uy*dvdy)
vdvdzms =  (uy*dvdz)
vdwdxms =  (uy*dwdx)
vdwdyms =  (uy*dwdy)
vdwdzms =  (uy*dwdz)



dudxx2ms =  (dudxx*dudxx)   ! uu
dudyy2ms =  (dudyy*dudyy)
dudzz2ms =  (dudzz*dudzz)
dudxy2ms =  (dudxy*dudxy)
dudxz2ms =  (dudxz*dudxz)
dudyz2ms =  (dudyz*dudyz)

dvdxx2ms =  (dvdxx*dvdxx)   ! vv
dvdyy2ms =  (dvdyy*dvdyy)
dvdzz2ms =  (dvdzz*dvdzz)
dvdxy2ms =  (dvdxy*dvdxy)
dvdxz2ms =  (dvdxz*dvdxz)
dvdyz2ms =  (dvdyz*dvdyz)

dwdxx2ms =  (dwdxx*dwdxx)   ! ww
dwdyy2ms =  (dwdyy*dwdyy)
dwdzz2ms =  (dwdzz*dwdzz)
dwdxy2ms =  (dwdxy*dwdxy)
dwdxz2ms =  (dwdxz*dwdxz)
dwdyz2ms =  (dwdyz*dwdyz)

dudvdxx2ms =  (dudxx*dvdxx)   ! uv
dudvdyy2ms =  (dudyy*dvdyy)
dudvdzz2ms =  (dudzz*dvdzz)
dudvdxy2ms =  (dudxy*dvdxy)
dudvdxz2ms =  (dudxz*dvdxz)
dudvdyz2ms =  (dudyz*dvdyz)

presms  =  (pres)  
pres2ms =  (pres*pres)

end subroutine update_user_stats_spat
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
subroutine update_user_stats_spat_oui_phi(ux,uy,uz,dpdx,dpdy,dpdz,&
						dudt,dvdt,dwdt,&
						dudx,dudy,dudz,&
						dvdx,dvdy,dvdz,&
						dwdx,dwdy,dwdz,&
						dudxx, dudyy, dudzz, dudxy, dudxz, dudyz,&
						dvdxx, dvdyy, dvdzz, dvdxy, dvdxz, dvdyz,&
						dwdxx, dwdyy, dwdzz, dwdxy, dwdxz, dwdyz,&
						pres,pp3,&
						phi,&
						dtdx,dtdy,dtdz,&
						dtdxx,dtdyy,dtdzz,dtdxy,dtdxz,dtdyz,&
						dtdxxx,dtdxyy,dtdxzz,&
						dtdyxx,dtdyyy,dtdyzz,&
						dtdzxx,dtdzyy,dtdzzz,&
						phG,ph1,ph2,ph3,ph4)

use decomp_2d, only : xsize, DECOMP_INFO
use decomp_2d_mpi, only : nrank    
use param, only : itime

implicit none

TYPE(DECOMP_INFO), intent(in) :: phG,ph1,ph2,ph3,ph4
real(mytype), dimension(xsize(1),xsize(2),xsize(3)), intent(in) :: ux,uy,uz,&
						dpdx, dpdy, dpdz,&
						dudt, dvdt, dwdt,&
						dudx, dudy, dudz,&
						dvdx, dvdy, dvdz,&
						dwdx, dwdy, dwdz,&
						dudxx, dudyy, dudzz, dudxy, dudxz, dudyz,&
						dvdxx, dvdyy, dvdzz, dvdxy, dvdxz, dvdyz,&
						dwdxx, dwdyy, dwdzz, dwdxy, dwdxz, dwdyz,&
						pres
real(mytype), dimension(ph3%zst(1):ph3%zen(1),ph3%zst(2):ph3%zen(2),ph3%zst(3):ph3%zen(3)), intent(in) :: pp3
real(mytype), dimension(xsize(1),xsize(2),xsize(3)), intent(in) :: phi,&
						dtdx,dtdy,dtdz,&
						dtdxx,dtdyy,dtdzz,dtdxy,dtdxz,dtdyz,&
						dtdxxx,dtdxyy,dtdxzz,&
						dtdyxx,dtdyyy,dtdyzz,&
						dtdzxx,dtdzyy,dtdzzz

! Local variable
integer :: coeff

!coeff=itime-beg_stat
!if (coeff.le.0) then
!  if (nrank.eq.0) print*,'ERROR : cannot start to compute statistics at ',itime,'. Wait till ',beg_stat
!  return
!endif

! First compute the velocity statistics
call update_user_stats_spat(ux,uy,uz,dpdx,dpdy,dpdz,&
				dudt,dvdt,dwdt,&
				dudx,dudy,dudz,&
				dvdx,dvdy,dvdz,&
				dwdx,dwdy,dwdz,&
				dudxx,dudyy,dudzz,dudxy,dudxz,dudyz,&
				dvdxx,dvdyy,dvdzz,dvdxy,dvdxz,dvdyz,&
				dwdxx,dwdyy,dwdzz,dwdxy,dwdxz,dwdyz,&
				pres,pp3,&
				phG,ph1,ph2,ph3,ph4)

! Ordre 1
phims = phi
dphidyms = dtdy
dphidxms = dtdx
dphidzms = dtdz
dphidyyms = dtdyy
dphidyyyms = dtdyyy

! Ordre 2
uphims = ux*phi
vphims = uy*phi
wphims = uz*phi
phiphims = phi*phi

phidpdxms = phi*dpdx
phidpdyms = phi*dpdy
phidpdzms = phi*dpdz

dphidx2ms = dtdx*dtdx
dphidy2ms = dtdy*dtdy
dphidz2ms = dtdz*dtdz

dudphidxms = dudx*dtdx
dvdphidxms = dvdx*dtdx
dwdphidxms = dwdx*dtdx

dudphidyms = dudy*dtdy
dvdphidyms = dvdy*dtdy
dwdphidyms = dwdy*dtdy

dudphidzms = dudz*dtdz
dvdphidzms = dvdz*dtdz
dwdphidzms = dwdz*dtdz

udtdxxms = ux*dtdxx
udtdyyms = ux*dtdyy
udtdzzms = ux*dtdzz

vdtdxxms = uy*dtdxx
vdtdyyms = uy*dtdyy
vdtdzzms = uy*dtdzz

wdtdxxms = uz*dtdxx
wdtdyyms = uz*dtdyy
wdtdzzms = uz*dtdzz

! Ordre 3
uphi2ms = ux*phi*phi
vphi2ms = uy*phi*phi
wphi2ms = uz*phi*phi

phiuums = ux*ux*phi
phivvms = uy*uy*phi
phiwwms = uz*uz*phi

phiuvms = ux*uy*phi
phiuwms = ux*uz*phi
phivwms = uy*uz*phi

! P2 : Production mixte gradU*gradT
dtdxdtdyms = dtdx*dtdy
dtdxdtdzms = dtdx*dtdz
dtdydtdzms = dtdy*dtdz

dtdxdudyms = dtdx*dudy
dtdydudxms = dtdy*dudx
dtdxdudzms = dtdx*dudz
dtdzdudxms = dtdz*dudx
dtdydudzms = dtdy*dudz
dtdzdudyms = dtdz*dudy

dtdxdvdyms = dtdx*dvdy
dtdydvdxms = dtdy*dvdx
dtdxdvdzms = dtdx*dvdz
dtdzdvdxms = dtdz*dvdx
dtdydvdzms = dtdy*dvdz
dtdzdvdyms = dtdz*dvdy

dtdxdwdyms = dtdx*dwdy
dtdydwdxms = dtdy*dwdx
dtdxdwdzms = dtdx*dwdz
dtdzdwdxms = dtdz*dwdx
dtdydwdzms = dtdy*dwdz
dtdzdwdyms = dtdz*dwdy

! P3 : Production du gradient (hessienne vitesse et temp.)
vdtdxms = uy*dtdx
vdtdyms = uy*dtdy
vdtdzms = uy*dtdz

! Dissipation xT
dudtdxxms = dudxx*dtdxx
dudtdyyms = dudyy*dtdyy
dudtdzzms = dudzz*dtdzz
dudtdxyms = dudxy*dtdxy
dudtdxzms = dudxz*dtdxz
dudtdyzms = dudyz*dtdyz

dudx_dtdxxxms = dudx*dtdxxx
dudx_dtdyxxms = dudx*dtdyxx
dudx_dtdzxxms = dudx*dtdzxx
dudy_dtdxyyms = dudy*dtdxyy
dudy_dtdyyyms = dudy*dtdyyy
dudy_dtdzyyms = dudy*dtdzyy
dudz_dtdxzzms = dudz*dtdxzz
dudz_dtdyzzms = dudz*dtdyzz
dudz_dtdzzms = dudz*dtdzzz

! Dissipation yT
dvdtdxxms = dvdxx*dtdxx
dvdtdyyms = dvdyy*dtdyy
dvdtdzzms = dvdzz*dtdzz
dvdtdxyms = dvdxy*dtdxy
dvdtdxzms = dvdxz*dtdxz
dvdtdyzms = dvdyz*dtdyz

dvdx_dtdxxxms = dvdx*dtdxxx
dvdx_dtdyxxms = dvdx*dtdyxx
dvdx_dtdzxxms = dvdx*dtdzxx
dvdy_dtdxyyms = dvdy*dtdxyy
dvdy_dtdyyyms = dvdy*dtdyyy
dvdy_dtdzyyms = dvdy*dtdzyy
dvdz_dtdxzzms = dvdz*dtdxzz
dvdz_dtdyzzms = dvdz*dtdyzz
dvdz_dtdzzms = dvdz*dtdzzz

! Dissipation TT
dtdxx2ms = dtdxx*dtdxx
dtdyy2ms = dtdyy*dtdyy
dtdzz2ms = dtdzz*dtdzz
dtdxy2ms = dtdxy*dtdxy
dtdxz2ms = dtdxz*dtdxz
dtdyz2ms = dtdyz*dtdyz

end subroutine update_user_stats_spat_oui_phi
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

subroutine update_user_stats_oui_phi(ux,uy,uz,dpdx,dpdy,dpdz,&
						dudx,dudy,dudz,&
						dvdx,dvdy,dvdz,&
						dwdx,dwdy,dwdz,&
						dudxx, dudyy, dudzz, dudxy, dudxz, dudyz,&
						dvdxx, dvdyy, dvdzz, dvdxy, dvdxz, dvdyz,&
						dwdxx, dwdyy, dwdzz, dwdxy, dwdxz, dwdyz,&
						pres,pp3,&
						phi,&
						dtdx,dtdy,dtdz,&
						dtdxx,dtdyy,dtdzz,dtdxy,dtdxz,dtdyz,&
						dtdxxx,dtdxyy,dtdxzz,&
						dtdyxx,dtdyyy,dtdyzz,&
						dtdzxx,dtdzyy,dtdzzz,&
						phG,ph1,ph2,ph3,ph4)

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! use decomp_2d, only : xsize, nrank, DECOMP_INFO
use decomp_2d, only : xsize, DECOMP_INFO
use decomp_2d_mpi, only : nrank            
use param, only : itime
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

implicit none

TYPE(DECOMP_INFO), intent(in) :: phG,ph1,ph2,ph3,ph4
real(mytype), dimension(xsize(1),xsize(2),xsize(3)), intent(in) :: ux,uy,uz,&
						dpdx, dpdy, dpdz,&
						dudx, dudy, dudz,&
						dvdx, dvdy, dvdz,&
						dwdx, dwdy, dwdz,&
						dudxx, dudyy, dudzz, dudxy, dudxz, dudyz,&
						dvdxx, dvdyy, dvdzz, dvdxy, dvdxz, dvdyz,&
						dwdxx, dwdyy, dwdzz, dwdxy, dwdxz, dwdyz,&
						pres
real(mytype), dimension(:,:,:), intent(in) :: pp3
real(mytype), dimension(xsize(1),xsize(2),xsize(3)), intent(in) :: phi,&
						dtdx,dtdy,dtdz,&
						dtdxx,dtdyy,dtdzz,dtdxy,dtdxz,dtdyz,&
						dtdxxx,dtdxyy,dtdxzz,&
						dtdyxx,dtdyyy,dtdyzz,&
						dtdzxx,dtdzyy,dtdzzz

! Local variable
integer :: coeff

coeff=itime-beg_stat
if (coeff.le.0) then
  if (nrank.eq.0) print*,'ERROR : cannot start to compute statistics at ',itime,'. Wait till ',beg_stat
  return
endif

call update_user_stats(ux,uy,uz,dpdx,dpdy,dpdz,&
				dudx,dudy,dudz,&
				dvdx,dvdy,dvdz,&
				dwdx,dwdy,dwdz,&
				dudxx,dudyy,dudzz,dudxy,dudxz,dudyz,&
				dvdxx,dvdyy,dvdzz,dvdxy,dvdxz,dvdyz,&
				dwdxx,dwdyy,dwdzz,dwdxy,dwdxz,dwdyz,&
				pres,pp3,&
				phG,ph1,ph2,ph3,ph4)

phim = (real(coeff-1, kind=mytype)*phim + (phi)) / real(coeff, kind=mytype)
dphidym = (real(coeff-1, kind=mytype)*dphidym + (dtdy)) / real(coeff, kind=mytype)
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
dphidxm = (real(coeff-1, kind=mytype)*dphidxm + (dtdx)) / real(coeff, kind=mytype)
dphidzm = (real(coeff-1, kind=mytype)*dphidzm + (dtdz)) / real(coeff, kind=mytype)
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
dphidyym = (real(coeff-1, kind=mytype)*dphidyym + (dtdyy)) / real(coeff, kind=mytype)
dphidyyym = (real(coeff-1, kind=mytype)*dphidyyym + (dtdyyy)) / real(coeff, kind=mytype)

uphim = (real(coeff-1, kind=mytype)*uphim + (ux*phi)) / real(coeff, kind=mytype)
vphim = (real(coeff-1, kind=mytype)*vphim + (uy*phi)) / real(coeff, kind=mytype)
wphim = (real(coeff-1, kind=mytype)*wphim + (uz*phi)) / real(coeff, kind=mytype)
phiphim = (real(coeff-1, kind=mytype)*phiphim + (phi*phi)) / real(coeff, kind=mytype)

phidpdxm = (real(coeff-1, kind=mytype)*phidpdxm + (phi*dpdx)) / real(coeff, kind=mytype)
phidpdym = (real(coeff-1, kind=mytype)*phidpdym + (phi*dpdy)) / real(coeff, kind=mytype)
phidpdzm = (real(coeff-1, kind=mytype)*phidpdzm + (phi*dpdz)) / real(coeff, kind=mytype)

dphidx2m = (real(coeff-1, kind=mytype)*dphidx2m + (dtdx*dtdx)) / real(coeff, kind=mytype)
dphidy2m = (real(coeff-1, kind=mytype)*dphidy2m + (dtdy*dtdy)) / real(coeff, kind=mytype)
dphidz2m = (real(coeff-1, kind=mytype)*dphidz2m + (dtdz*dtdz)) / real(coeff, kind=mytype)
dudphidxm = (real(coeff-1, kind=mytype)*dudphidxm + (dudx*dtdx)) / real(coeff, kind=mytype)
dvdphidxm = (real(coeff-1, kind=mytype)*dvdphidxm + (dvdx*dtdx)) / real(coeff, kind=mytype)
dwdphidxm = (real(coeff-1, kind=mytype)*dwdphidxm + (dwdx*dtdx)) / real(coeff, kind=mytype)
dudphidym = (real(coeff-1, kind=mytype)*dudphidym + (dudy*dtdy)) / real(coeff, kind=mytype)
dvdphidym = (real(coeff-1, kind=mytype)*dvdphidym + (dvdy*dtdy)) / real(coeff, kind=mytype)
dwdphidym = (real(coeff-1, kind=mytype)*dwdphidym + (dwdy*dtdy)) / real(coeff, kind=mytype)
dudphidzm = (real(coeff-1, kind=mytype)*dudphidzm + (dudz*dtdz)) / real(coeff, kind=mytype)
dvdphidzm = (real(coeff-1, kind=mytype)*dvdphidzm + (dvdz*dtdz)) / real(coeff, kind=mytype)
dwdphidzm = (real(coeff-1, kind=mytype)*dwdphidzm + (dwdz*dtdz)) / real(coeff, kind=mytype)

udtdxxm = (real(coeff-1, kind=mytype)*udtdxxm + (ux*dtdxx)) / real(coeff, kind=mytype)
udtdyym = (real(coeff-1, kind=mytype)*udtdyym + (ux*dtdyy)) / real(coeff, kind=mytype)
udtdzzm = (real(coeff-1, kind=mytype)*udtdzzm + (ux*dtdzz)) / real(coeff, kind=mytype)

vdtdxxm = (real(coeff-1, kind=mytype)*vdtdxxm + (uy*dtdxx)) / real(coeff, kind=mytype)
vdtdyym = (real(coeff-1, kind=mytype)*vdtdyym + (uy*dtdyy)) / real(coeff, kind=mytype)
vdtdzzm = (real(coeff-1, kind=mytype)*vdtdzzm + (uy*dtdzz)) / real(coeff, kind=mytype)

wdtdxxm = (real(coeff-1, kind=mytype)*wdtdxxm + (uz*dtdxx)) / real(coeff, kind=mytype)
wdtdyym = (real(coeff-1, kind=mytype)*wdtdyym + (uz*dtdyy)) / real(coeff, kind=mytype)
wdtdzzm = (real(coeff-1, kind=mytype)*wdtdzzm + (uz*dtdzz)) / real(coeff, kind=mytype)

uphi2m = (real(coeff-1, kind=mytype)*uphi2m + (ux*phi*phi)) / real(coeff, kind=mytype)
vphi2m = (real(coeff-1, kind=mytype)*vphi2m + (uy*phi*phi)) / real(coeff, kind=mytype)
wphi2m = (real(coeff-1, kind=mytype)*wphi2m + (uz*phi*phi)) / real(coeff, kind=mytype)
phiuum = (real(coeff-1, kind=mytype)*phiuum + (ux*ux*phi)) / real(coeff, kind=mytype)
phivvm = (real(coeff-1, kind=mytype)*phivvm + (uy*uy*phi)) / real(coeff, kind=mytype)
phiwwm = (real(coeff-1, kind=mytype)*phiwwm + (uz*uz*phi)) / real(coeff, kind=mytype)
phiuvm = (real(coeff-1, kind=mytype)*phiuvm + (ux*uy*phi)) / real(coeff, kind=mytype)
phiuwm = (real(coeff-1, kind=mytype)*phiuwm + (ux*uz*phi)) / real(coeff, kind=mytype)
phivwm = (real(coeff-1, kind=mytype)*phivwm + (uy*uz*phi)) / real(coeff, kind=mytype)

dtdxdtdym = (real(coeff-1, kind=mytype)*dtdxdtdym + (dtdx*dtdy)) / real(coeff, kind=mytype)
dtdxdtdzm = (real(coeff-1, kind=mytype)*dtdxdtdzm + (dtdx*dtdz)) / real(coeff, kind=mytype)
dtdydtdzm = (real(coeff-1, kind=mytype)*dtdydtdzm + (dtdy*dtdz)) / real(coeff, kind=mytype)

dtdxdudym = (real(coeff-1, kind=mytype)*dtdxdudym + (dtdx*dudy)) / real(coeff, kind=mytype)
dtdydudxm = (real(coeff-1, kind=mytype)*dtdydudxm + (dtdy*dudx)) / real(coeff, kind=mytype)
dtdxdudzm = (real(coeff-1, kind=mytype)*dtdxdudzm + (dtdx*dudz)) / real(coeff, kind=mytype)
dtdzdudxm = (real(coeff-1, kind=mytype)*dtdzdudxm + (dtdz*dudx)) / real(coeff, kind=mytype)
dtdydudzm = (real(coeff-1, kind=mytype)*dtdydudzm + (dtdy*dudz)) / real(coeff, kind=mytype)
dtdzdudym = (real(coeff-1, kind=mytype)*dtdzdudym + (dtdz*dudy)) / real(coeff, kind=mytype)

dtdxdvdym = (real(coeff-1, kind=mytype)*dtdxdvdym + (dtdx*dvdy)) / real(coeff, kind=mytype)
dtdydvdxm = (real(coeff-1, kind=mytype)*dtdydvdxm + (dtdy*dvdx)) / real(coeff, kind=mytype)
dtdxdvdzm = (real(coeff-1, kind=mytype)*dtdxdvdzm + (dtdx*dvdz)) / real(coeff, kind=mytype)
dtdzdvdxm = (real(coeff-1, kind=mytype)*dtdzdvdxm + (dtdz*dvdx)) / real(coeff, kind=mytype)
dtdydvdzm = (real(coeff-1, kind=mytype)*dtdydvdzm + (dtdy*dvdz)) / real(coeff, kind=mytype)
dtdzdvdym = (real(coeff-1, kind=mytype)*dtdzdvdym + (dtdz*dvdy)) / real(coeff, kind=mytype)

dtdxdwdym = (real(coeff-1, kind=mytype)*dtdxdwdym + (dtdx*dwdy)) / real(coeff, kind=mytype)
dtdydwdxm = (real(coeff-1, kind=mytype)*dtdydwdxm + (dtdy*dwdx)) / real(coeff, kind=mytype)
dtdxdwdzm = (real(coeff-1, kind=mytype)*dtdxdwdzm + (dtdx*dwdz)) / real(coeff, kind=mytype)
dtdzdwdxm = (real(coeff-1, kind=mytype)*dtdzdwdxm + (dtdz*dwdx)) / real(coeff, kind=mytype)
dtdydwdzm = (real(coeff-1, kind=mytype)*dtdydwdzm + (dtdy*dwdz)) / real(coeff, kind=mytype)
dtdzdwdym = (real(coeff-1, kind=mytype)*dtdzdwdym + (dtdz*dwdy)) / real(coeff, kind=mytype)

vdtdxm = (real(coeff-1, kind=mytype)*vdtdxm + (uy*dtdx)) / real(coeff, kind=mytype)
vdtdym = (real(coeff-1, kind=mytype)*vdtdym + (uy*dtdy)) / real(coeff, kind=mytype)
vdtdzm = (real(coeff-1, kind=mytype)*vdtdzm + (uy*dtdz)) / real(coeff, kind=mytype)

dudx_dudtdxm = (real(coeff-1, kind=mytype)*dudx_dudtdxm + (dudx*dudx*dtdx)) / real(coeff, kind=mytype)
dudx_dudtdym = (real(coeff-1, kind=mytype)*dudx_dudtdym + (dudx*dudy*dtdy)) / real(coeff, kind=mytype)
dudx_dudtdzm = (real(coeff-1, kind=mytype)*dudx_dudtdzm + (dudx*dudz*dtdz)) / real(coeff, kind=mytype)
dudy_dvdtdxm = (real(coeff-1, kind=mytype)*dudy_dvdtdxm + (dudy*dvdx*dtdx)) / real(coeff, kind=mytype)
dudy_dvdtdym = (real(coeff-1, kind=mytype)*dudy_dvdtdym + (dudy*dvdy*dtdy)) / real(coeff, kind=mytype)
dudy_dvdtdzm = (real(coeff-1, kind=mytype)*dudy_dvdtdzm + (dudy*dvdz*dtdz)) / real(coeff, kind=mytype)
dudz_dwdtdxm = (real(coeff-1, kind=mytype)*dudz_dwdtdxm + (dudz*dwdx*dtdx)) / real(coeff, kind=mytype)
dudz_dwdtdym = (real(coeff-1, kind=mytype)*dudz_dwdtdym + (dudz*dwdy*dtdy)) / real(coeff, kind=mytype)
dudz_dwdtdzm = (real(coeff-1, kind=mytype)*dudz_dwdtdzm + (dudz*dwdz*dtdz)) / real(coeff, kind=mytype)

dtdx_dududym = (real(coeff-1, kind=mytype)*dtdx_dududym + (dtdx*dudy*dudy)) / real(coeff, kind=mytype)
dtdx_dududzm = (real(coeff-1, kind=mytype)*dtdx_dududzm + (dtdx*dudz*dudz)) / real(coeff, kind=mytype)
dtdy_dudvdxm = (real(coeff-1, kind=mytype)*dtdy_dudvdxm + (dtdy*dudx*dvdx)) / real(coeff, kind=mytype)
dtdy_dudvdzm = (real(coeff-1, kind=mytype)*dtdy_dudvdzm + (dtdy*dudz*dvdz)) / real(coeff, kind=mytype)
dtdz_dudwdxm = (real(coeff-1, kind=mytype)*dtdz_dudwdxm + (dtdz*dudx*dwdx)) / real(coeff, kind=mytype)
dtdz_dudwdym = (real(coeff-1, kind=mytype)*dtdz_dudwdym + (dtdz*dudy*dwdy)) / real(coeff, kind=mytype)

dvdx_dudtdxm = (real(coeff-1, kind=mytype)*dvdx_dudtdxm + (dvdx*dudx*dtdx)) / real(coeff, kind=mytype)
dvdx_dudtdym = (real(coeff-1, kind=mytype)*dvdx_dudtdym + (dvdx*dudy*dtdy)) / real(coeff, kind=mytype)
dvdx_dudtdzm = (real(coeff-1, kind=mytype)*dvdx_dudtdzm + (dvdx*dudz*dtdz)) / real(coeff, kind=mytype)
dvdy_dvdtdxm = (real(coeff-1, kind=mytype)*dvdy_dvdtdxm + (dvdy*dvdx*dtdx)) / real(coeff, kind=mytype)
dvdy_dvdtdym = (real(coeff-1, kind=mytype)*dvdy_dvdtdym + (dvdy*dvdy*dtdy)) / real(coeff, kind=mytype)
dvdy_dvdtdzm = (real(coeff-1, kind=mytype)*dvdy_dvdtdzm + (dvdy*dvdz*dtdz)) / real(coeff, kind=mytype)
dvdz_dwdtdxm = (real(coeff-1, kind=mytype)*dvdz_dwdtdxm + (dvdz*dwdx*dtdx)) / real(coeff, kind=mytype)
dvdz_dwdtdym = (real(coeff-1, kind=mytype)*dvdz_dwdtdym + (dvdz*dwdy*dtdy)) / real(coeff, kind=mytype)
dvdz_dwdtdzm = (real(coeff-1, kind=mytype)*dvdz_dwdtdzm + (dvdz*dwdz*dtdz)) / real(coeff, kind=mytype)

dtdx_dudvdym = (real(coeff-1, kind=mytype)*dtdx_dudvdym + (dtdx*dudy*dvdy)) / real(coeff, kind=mytype)
dtdx_dudvdzm = (real(coeff-1, kind=mytype)*dtdx_dudvdzm + (dtdx*dudz*dvdz)) / real(coeff, kind=mytype)
dtdy_dvdvdxm = (real(coeff-1, kind=mytype)*dtdy_dvdvdxm + (dtdy*dvdx*dvdx)) / real(coeff, kind=mytype)
dtdy_dvdvdzm = (real(coeff-1, kind=mytype)*dtdy_dvdvdzm + (dtdy*dvdz*dvdz)) / real(coeff, kind=mytype)
dtdz_dvdwdxm = (real(coeff-1, kind=mytype)*dtdz_dvdwdxm + (dtdz*dvdx*dwdx)) / real(coeff, kind=mytype)
dtdz_dvdwdym = (real(coeff-1, kind=mytype)*dtdz_dvdwdym + (dtdz*dvdy*dwdy)) / real(coeff, kind=mytype)

dtdx_dudtdxm = (real(coeff-1, kind=mytype)*dtdx_dudtdxm + (dtdx*dudx*dtdx)) / real(coeff, kind=mytype)
dtdx_dudtdym = (real(coeff-1, kind=mytype)*dtdx_dudtdym + (dtdx*dudy*dtdy)) / real(coeff, kind=mytype)
dtdx_dudtdzm = (real(coeff-1, kind=mytype)*dtdx_dudtdzm + (dtdx*dudz*dtdz)) / real(coeff, kind=mytype)
dtdy_dvdtdxm = (real(coeff-1, kind=mytype)*dtdy_dvdtdxm + (dtdy*dvdx*dtdx)) / real(coeff, kind=mytype)
dtdy_dvdtdym = (real(coeff-1, kind=mytype)*dtdy_dvdtdym + (dtdy*dvdy*dtdy)) / real(coeff, kind=mytype)
dtdy_dvdtdzm = (real(coeff-1, kind=mytype)*dtdy_dvdtdzm + (dtdy*dvdz*dtdz)) / real(coeff, kind=mytype)
dtdz_dwdtdxm = (real(coeff-1, kind=mytype)*dtdz_dwdtdxm + (dtdz*dwdx*dtdx)) / real(coeff, kind=mytype)
dtdz_dwdtdym = (real(coeff-1, kind=mytype)*dtdz_dwdtdym + (dtdz*dwdy*dtdy)) / real(coeff, kind=mytype)
dtdz_dwdtdzm = (real(coeff-1, kind=mytype)*dtdz_dwdtdzm + (dtdz*dwdz*dtdz)) / real(coeff, kind=mytype)

vdudtdxm = (real(coeff-1, kind=mytype)*vdudtdxm + (uy*dudx*dtdx)) / real(coeff, kind=mytype)
vdudtdym = (real(coeff-1, kind=mytype)*vdudtdym + (uy*dudy*dtdy)) / real(coeff, kind=mytype)
vdudtdzm = (real(coeff-1, kind=mytype)*vdudtdzm + (uy*dudz*dtdz)) / real(coeff, kind=mytype)
vdvdtdxm = (real(coeff-1, kind=mytype)*vdvdtdxm + (uy*dvdx*dtdx)) / real(coeff, kind=mytype)
vdvdtdym = (real(coeff-1, kind=mytype)*vdvdtdym + (uy*dvdy*dtdy)) / real(coeff, kind=mytype)
vdvdtdzm = (real(coeff-1, kind=mytype)*vdvdtdzm + (uy*dvdz*dtdz)) / real(coeff, kind=mytype)
vdtdx2m = (real(coeff-1, kind=mytype)*vdtdx2m + (uy*dtdx*dtdx)) / real(coeff, kind=mytype)
vdtdy2m = (real(coeff-1, kind=mytype)*vdtdy2m + (uy*dtdy*dtdy)) / real(coeff, kind=mytype)
vdtdz2m = (real(coeff-1, kind=mytype)*vdtdz2m + (uy*dtdz*dtdz)) / real(coeff, kind=mytype)

dpdtdxm = (real(coeff-1, kind=mytype)*dpdtdxm + (dpdx*dtdx)) / real(coeff, kind=mytype)
dpdtdym = (real(coeff-1, kind=mytype)*dpdtdym + (dpdy*dtdy)) / real(coeff, kind=mytype)
dpdtdzm = (real(coeff-1, kind=mytype)*dpdtdzm + (dpdz*dtdz)) / real(coeff, kind=mytype)
dpdtdxxm = (real(coeff-1, kind=mytype)*dpdtdxxm + (dpdx*dtdxx)) / real(coeff, kind=mytype)
dpdtdxym = (real(coeff-1, kind=mytype)*dpdtdxym + (dpdx*dtdxy)) / real(coeff, kind=mytype)
dpdtdxzm = (real(coeff-1, kind=mytype)*dpdtdxzm + (dpdx*dtdxz)) / real(coeff, kind=mytype)
dpdtdyxm = (real(coeff-1, kind=mytype)*dpdtdyxm + (dpdy*dtdxy)) / real(coeff, kind=mytype)
dpdtdyym = (real(coeff-1, kind=mytype)*dpdtdyym + (dpdy*dtdyy)) / real(coeff, kind=mytype)
dpdtdyzm = (real(coeff-1, kind=mytype)*dpdtdyzm + (dpdy*dtdyz)) / real(coeff, kind=mytype)

dudtdxxm = (real(coeff-1, kind=mytype)*dudtdxxm + (dudxx*dtdxx)) / real(coeff, kind=mytype)
dudtdyym = (real(coeff-1, kind=mytype)*dudtdyym + (dudyy*dtdyy)) / real(coeff, kind=mytype)
dudtdzzm = (real(coeff-1, kind=mytype)*dudtdzzm + (dudzz*dtdzz)) / real(coeff, kind=mytype)
dudtdxym = (real(coeff-1, kind=mytype)*dudtdxym + (dudxy*dtdxy)) / real(coeff, kind=mytype)
dudtdxzm = (real(coeff-1, kind=mytype)*dudtdxzm + (dudxz*dtdxz)) / real(coeff, kind=mytype)
dudtdyzm = (real(coeff-1, kind=mytype)*dudtdyzm + (dudyz*dtdyz)) / real(coeff, kind=mytype)

dudx_dtdxxxm = (real(coeff-1, kind=mytype)*dudx_dtdxxxm + (dudx*dtdxxx)) / real(coeff, kind=mytype)
dudx_dtdyxxm = (real(coeff-1, kind=mytype)*dudx_dtdyxxm + (dudx*dtdyxx)) / real(coeff, kind=mytype)
dudx_dtdzxxm = (real(coeff-1, kind=mytype)*dudx_dtdzxxm + (dudx*dtdzxx)) / real(coeff, kind=mytype)
dudy_dtdxyym = (real(coeff-1, kind=mytype)*dudy_dtdxyym + (dudy*dtdxyy)) / real(coeff, kind=mytype)
dudy_dtdyyym = (real(coeff-1, kind=mytype)*dudy_dtdyyym + (dudy*dtdyyy)) / real(coeff, kind=mytype)
dudy_dtdzyym = (real(coeff-1, kind=mytype)*dudy_dtdzyym + (dudy*dtdzyy)) / real(coeff, kind=mytype)
dudz_dtdxzzm = (real(coeff-1, kind=mytype)*dudz_dtdxzzm + (dudz*dtdxzz)) / real(coeff, kind=mytype)
dudz_dtdyzzm = (real(coeff-1, kind=mytype)*dudz_dtdyzzm + (dudz*dtdyzz)) / real(coeff, kind=mytype)
dudz_dtdzzzm = (real(coeff-1, kind=mytype)*dudz_dtdzzzm + (dudz*dtdzzz)) / real(coeff, kind=mytype)

dvdtdxxm = (real(coeff-1, kind=mytype)*dvdtdxxm + (dvdxx*dtdxx)) / real(coeff, kind=mytype)
dvdtdyym = (real(coeff-1, kind=mytype)*dvdtdyym + (dvdyy*dtdyy)) / real(coeff, kind=mytype)
dvdtdzzm = (real(coeff-1, kind=mytype)*dvdtdzzm + (dvdzz*dtdzz)) / real(coeff, kind=mytype)
dvdtdxym = (real(coeff-1, kind=mytype)*dvdtdxym + (dvdxy*dtdxy)) / real(coeff, kind=mytype)
dvdtdxzm = (real(coeff-1, kind=mytype)*dvdtdxzm + (dvdxz*dtdxz)) / real(coeff, kind=mytype)
dvdtdyzm = (real(coeff-1, kind=mytype)*dvdtdyzm + (dvdyz*dtdyz)) / real(coeff, kind=mytype)

dvdx_dtdxxxm = (real(coeff-1, kind=mytype)*dvdx_dtdxxxm + (dvdx*dtdxxx)) / real(coeff, kind=mytype)
dvdx_dtdyxxm = (real(coeff-1, kind=mytype)*dvdx_dtdyxxm + (dvdx*dtdyxx)) / real(coeff, kind=mytype)
dvdx_dtdzxxm = (real(coeff-1, kind=mytype)*dvdx_dtdzxxm + (dvdx*dtdzxx)) / real(coeff, kind=mytype)
dvdy_dtdxyym = (real(coeff-1, kind=mytype)*dvdy_dtdxyym + (dvdy*dtdxyy)) / real(coeff, kind=mytype)
dvdy_dtdyyym = (real(coeff-1, kind=mytype)*dvdy_dtdyyym + (dvdy*dtdyyy)) / real(coeff, kind=mytype)
dvdy_dtdzyym = (real(coeff-1, kind=mytype)*dvdy_dtdzyym + (dvdy*dtdzyy)) / real(coeff, kind=mytype)
dvdz_dtdxzzm = (real(coeff-1, kind=mytype)*dvdz_dtdxzzm + (dvdz*dtdxzz)) / real(coeff, kind=mytype)
dvdz_dtdyzzm = (real(coeff-1, kind=mytype)*dvdz_dtdyzzm + (dvdz*dtdyzz)) / real(coeff, kind=mytype)
dvdz_dtdzzzm = (real(coeff-1, kind=mytype)*dvdz_dtdzzzm + (dvdz*dtdzzz)) / real(coeff, kind=mytype)

dtdxx2m = (real(coeff-1, kind=mytype)*dtdxx2m + (dtdxx*dtdxx)) / real(coeff, kind=mytype)
dtdyy2m = (real(coeff-1, kind=mytype)*dtdyy2m + (dtdyy*dtdyy)) / real(coeff, kind=mytype)
dtdzz2m = (real(coeff-1, kind=mytype)*dtdzz2m + (dtdzz*dtdzz)) / real(coeff, kind=mytype)
dtdxy2m = (real(coeff-1, kind=mytype)*dtdxy2m + (dtdxy*dtdxy)) / real(coeff, kind=mytype)
dtdxz2m = (real(coeff-1, kind=mytype)*dtdxz2m + (dtdxz*dtdxz)) / real(coeff, kind=mytype)
dtdyz2m = (real(coeff-1, kind=mytype)*dtdyz2m + (dtdyz*dtdyz)) / real(coeff, kind=mytype)

end subroutine update_user_stats_oui_phi

subroutine pre_update_user_stats(phGin,ph1in,ph2in,ph3in,ph4in)

  use decomp_2d_io, only : decomp_2d_write_plane
  use decomp_2d, only : xsize, ysize, zsize, DECOMP_INFO, get_decomp_info
  use param, only : istret, iscalar, zero, iimplicit, icalc_2pc
  use variables
  !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ! recall that in variables lives phi1(:,:,:,:)... so to transpose only phi1(:,:,:,1)?
  ! this imports ph1, ph2, ph3, ph4, phG... they are defined in decomp2d.f90
  ! so change ph1, ph2, ph3, ph4, phG intent in to ph1in, ph2in, ph3in, ph4in, phGin
  ! other option is delete phG, ph1, ph2, .etc.. Var has also ph1, ph2, etc.
  use var
  !xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  implicit none

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  
!  TYPE(DECOMP_INFO), intent(in) :: phG,ph1,ph2,ph3,ph4 ! phG,ph1,ph2,ph3,ph4 are defined in decomp2d.f90 and imported in var
    TYPE(DECOMP_INFO), intent(in) :: phGin,ph1in,ph2in,ph3in,ph4in ! 
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  
  logical :: myflag
  integer :: i,j,k

  ! Pressure interpolation
  real(mytype), dimension(xsize(1),xsize(2),xsize(3)) :: tmp_pres
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  !TYPE(DECOMP_INFO) :: decomp_main ! this is defined in decomp2d.f90 and imported in var.
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  
  character(len=10) :: timer

  real(mytype),dimension(ysize(1),8,ysize(3)) :: output_tdyt

  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dudx, dudy, dudz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dvdx, dvdy, dvdz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dwdx, dwdy, dwdz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dudxx,dudyy,dudzz,dudxy,dudxz,dudyz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dvdxx,dvdyy,dvdzz,dvdxy,dvdxz,dvdyz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dwdxx,dwdyy,dwdzz,dwdxy,dwdxz,dwdyz

  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdx, dtdy, dtdz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdxx,dtdyy,dtdzz,dtdxy,dtdxz,dtdyz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdxxx,dtdxyy,dtdxzz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdyxx,dtdyyy,dtdyzz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdzxx,dtdzyy,dtdzzz

TYPE(DECOMP_INFO) :: phglob

call get_decomp_info(phglob)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ! this if for the case when iimplicit=1, in this case we do not want to calculate gradients with iimplicit.
  myflag=.false. ! No se para que usar myflag y iimplicit
  if (iimplicit.eq.1) myflag=.true.
  if (myflag) iimplicit=0
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  ! Compute laplacian temperature (budget uPhi, vPhi, wPhi) and grad(phi)
  if (iscalar.eq.1) then

    ! Compute laplacian phi
    !
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx    
    call transpose_x_to_y(phi1(:,:,:,1),phi2(:,:,:,1),phglob)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx    
    call transpose_y_to_z(phi2(:,:,:,1),phi3(:,:,:,1))
    !
    call derx(dtdx,phi1(:,:,:,1),di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx	
    call derxx (dtdxx,phi1(:,:,:,1),di1,sx,sfx,ssx,swx,xsize(1),xsize(2),xsize(3),0,zero)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    call derx(dtdxxx,dtdxx,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero)
    !
    call dery (tb2,phi2(:,:,:,1),di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
    call transpose_y_to_x(tb2,dtdy) ! dTdy
    call derx(dtdxy,dtdy,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero) ! dTdxy

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx	
    !call derxxt(dtdyxx,dtdy,di1,sx,sfxt,ssxt,swxt,xsize(1),xsize(2),xsize(3),0) ! dTdyxx
    call derxx (dtdyxx,dtdy,di1,sx,sfx,ssx,swx,xsize(1),xsize(2),xsize(3),0,zero)
!
    !call deryyt(ta2,phi2,di2,sy,sfyt,ssyt,swyt,ysize(1),ysize(2),ysize(3),0)
    call deryy (ta2,phi2(:,:,:,1),di2,sy,sfy,ssy,swy,ysize(1),ysize(2),ysize(3),0,zero)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    if (istret.ne.0) then 
      do j=1,ysize(2)
        ta2(:,j,:)=ta2(:,j,:)*pp2y(j)-pp4y(j)*tb2(:,j,:) ! dyy dans ta2
      enddo
    endif
    call transpose_y_to_x(ta2,dtdyy) ! dTdyy
    call derx(dtdxyy,dtdyy,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero) ! dTdxyy
    call dery (tc2,ta2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
    call transpose_y_to_x(tc2,dtdyyy) ! dTdyyy
    call transpose_y_to_z(ta2,ta3)
    call derz (tb3,ta3,di3,sz,ffz,fsz,fwz,zsize(1),zsize(2),zsize(3),0,zero)
    call transpose_z_to_y(tb3,ta2)
    call transpose_y_to_x(ta2,dtdzyy)
    !
    call derz (ta3,phi3(:,:,:,1),di3,sz,ffz,fsz,fwz,zsize(1),zsize(2),zsize(3),0,zero)
    call transpose_z_to_y(ta3,ta2) ! dTdz dans tb2
    call transpose_y_to_x(ta2,dtdz) ! dTdz
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    !call derxxt(dtdzxx,dtdz,di1,sx,sfxt,ssxt,swxt,xsize(1),xsize(2),xsize(3),0)
    call derxx (dtdzxx,dtdz,di1,sx,sfx,ssx,swx,xsize(1),xsize(2),xsize(3),0,zero)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    call dery (tc2,ta2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
    call transpose_y_to_x(tc2,dtdyz) ! dTdyz
    call derx(dtdxz,dtdz,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    !call derzzt (ta3,phi3,di3,sz,sfzt,sszt,swzt,zsize(1),zsize(2),zsize(3),0)
    call derzz (ta3,phi3(:,:,:,1),di3,sz,sfz,ssz,swz,zsize(1),zsize(2),zsize(3),0,zero)
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    call transpose_z_to_y(ta3,ta2) ! dzz dans ta2
    call transpose_y_to_x(ta2,dtdzz) ! dTdzz
    call derx(dtdxzz,dtdzz,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero)
    call dery (tc2,ta2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
    call transpose_y_to_x(tc2,dtdyzz)
    call derz (tb3,ta3,di3,sz,ffz,fsz,fwz,zsize(1),zsize(2),zsize(3),0,zero)
    call transpose_z_to_y(tb3,ta2)
    call transpose_y_to_x(ta2,dtdzzz)

!!!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! al parecer al insertar el modulo stat en mycode IC3D no corre bien con decomp_2d_write_plane! falta agregar islice algo asi de 4,5,6
!!!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!if (mod(itime,20).eq.0) then
!154 format('t',I9.9)
!  write(timer,154) itime
!  output_tdyt(:,1,:)=phi2(:,1,:)
!  output_tdyt(:,2,:)= tb2(:,1,:)
!  output_tdyt(:,3,:)=phi2(:,11,:)
!  output_tdyt(:,4,:)= tb2(:,11,:)
!  output_tdyt(:,5,:)=phi2(:,30,:)
!  output_tdyt(:,6,:)= tb2(:,30,:)
!  output_tdyt(:,7,:)=phi2(:,96,:)
!  output_tdyt(:,8,:)= tb2(:,96,:)
!  call decomp_2d_write_plane(2,output_tdyt,5,8,'slices/tdyt_flu_'//timer//'.dat')
!endif

  endif

  ! Compute velocity gradient
  call derx (dudx,ux1,di1,sx,ffx ,fsx ,fwx ,xsize(1),xsize(2),xsize(3),0,zero)
  call derx (dvdx,uy1,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dwdx,uz1,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  ! ta1=dudx / tb1=dvdx / tc1=dwdx
  call transpose_x_to_y(ux1,ux2)
  call transpose_x_to_y(uy1,uy2)
  call transpose_x_to_y(uz1,uz2)

  if (icalc_2pc.eq.1) then
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! we save in each plane (y pencil) u,v,w and phi2 in y^+=10, y=0.0565
  if (mod(itime,20).eq.0) then
  154 format('t',I9.9)
    write(timer,154) itime
    ! 2 is the y pencil, output_tdyt is the array to save, 2 is the iplane (2-xz plane),
    ! n is the plane to save, dirname, varname
    call decomp_2d_write_plane(2,ux2,2,ysize(2)/16+3,"slices",'u_flu_1'//timer//'.dat',"stats-flu-io")
    call decomp_2d_write_plane(2,phi2(:,:,:,1),2,ysize(2)/16+3,"slices",'phi_flu_1'//timer//'.dat',"stats-flu-io")
    call decomp_2d_write_plane(2,ux2,2,ysize(2)/2+1,"slices",'u_flu_2'//timer//'.dat',"stats-flu-io")
    call decomp_2d_write_plane(2,phi2(:,:,:,1),2,ysize(2)/2+1,"slices",'phi_flu_2'//timer//'.dat',"stats-flu-io")
    if (nrank.eq.0) print*,'inside ouput_tdyt: ', ysize(2)/16+3, ysize(2)/2+1
  endif
  endif
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


  call dery (ta2,ux2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call dery (tb2,uy2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
  call dery (tc2,uz2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call transpose_y_to_x(ta2,dudy)!dudy
  call transpose_y_to_x(tb2,dvdy)!dvdy
  call transpose_y_to_x(tc2,dwdy)!dwdy
! ta2=dudy / tb2=dvdy / tc2=dwdy
  call transpose_y_to_z(ux2,ux3)
  call transpose_y_to_z(uy2,uy3)
  call transpose_y_to_z(uz2,uz3)
  call derz (ta3,ux3,di3,sz,ffzp,fszp,fwzp,zsize(1),zsize(2),zsize(3),1,zero)
  call derz (tb3,uy3,di3,sz,ffzp,fszp,fwzp,zsize(1),zsize(2),zsize(3),1,zero)
  call derz (tc3,uz3,di3,sz,ffz ,fsz ,fwz ,zsize(1),zsize(2),zsize(3),0,zero)
  ! ta3=dudz / tb3=dvdz / tc3=dwdz
  ! Back in y
  call transpose_z_to_y(ta3,td2)!dudz
  call transpose_z_to_y(tb3,te2)!dvdz
  call transpose_z_to_y(tc3,tf2)!dwdz
  ! Back to x
  call transpose_y_to_x(td2,dudz)!dudz
  call transpose_y_to_x(te2,dvdz)!dvdz
  call transpose_y_to_x(tf2,dwdz)!dwdz

  ! Compute velocity second derivatives
  call derxx (dudxx,ux1,di1,sx,sfx ,ssx ,swx ,xsize(1),xsize(2),xsize(3),0,zero)
  call derx (dudxy,dudy,di1,sx,ffx ,fsx ,fwx ,xsize(1),xsize(2),xsize(3),0,zero)
  call derx (dudxz,dudz,di1,sx,ffx ,fsx ,fwx ,xsize(1),xsize(2),xsize(3),0,zero)
  call derxx (dvdxx,uy1,di1,sx,sfxp,ssxp,swxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dvdxy,dvdy,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dvdxz,dvdz,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derxx (dwdxx,uz1,di1,sx,sfxp,ssxp,swxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dwdxy,dwdy,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dwdxz,dwdz,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  !
  call dery (tg2,td2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call transpose_y_to_x(tg2,dudyz)!dudyz
  call dery (tg2,te2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call transpose_y_to_x(tg2,dvdyz)!dvdyz
  call dery (tg2,tf2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call transpose_y_to_x(tg2,dwdyz)!dwdyz
  !
  call deryy (td2,ux2,di2,sy,sfyp,ssyp,swyp,ysize(1),ysize(2),ysize(3),1,zero)
  do j=1,ysize(2)
    td2(:,j,:)=td2(:,j,:)*pp2y(j)-pp4y(j)*ta2(:,j,:)
  enddo
  call transpose_y_to_x(td2,dudyy)!dudyy
  call deryy (te2,uy2,di2,sy,sfy,ssy,swy,ysize(1),ysize(2),ysize(3),0,zero)
  do j=1,ysize(2)
    te2(:,j,:)=te2(:,j,:)*pp2y(j)-pp4y(j)*tb2(:,j,:)
  enddo
  call transpose_y_to_x(te2,dvdyy)!dvdyy
  call deryy (tf2,uz2,di2,sy,sfyp,ssyp,swyp,ysize(1),ysize(2),ysize(3),1,zero)
  do j=1,ysize(2)
    tf2(:,j,:)=tf2(:,j,:)*pp2y(j)-pp4y(j)*tc2(:,j,:)
  enddo
  call transpose_y_to_x(tf2,dwdyy)!dwdyy
  !
  call derzz (ta3,ux3,di3,sz,sfzp,sszp,swzp,zsize(1),zsize(2),zsize(3),1,zero)
  call derzz (tb3,uy3,di3,sz,sfzp,sszp,swzp,zsize(1),zsize(2),zsize(3),1,zero)
  call derzz (tc3,uz3,di3,sz,sfz ,ssz ,swz ,zsize(1),zsize(2),zsize(3),0,zero)
  call transpose_z_to_y(ta3,td2)!dudzz
  call transpose_z_to_y(tb3,te2)!dvdzz
  call transpose_z_to_y(tc3,tf2)!dwdzz
  call transpose_y_to_x(td2,dudzz)!dudzz
  call transpose_y_to_x(te2,dvdzz)!dvdzz
  call transpose_y_to_x(tf2,dwdzz)!dwdzz

  if (.true.) then
    !WORK Z-PENCILS
    call interzpv(ppi3,pp3(:,:,:,1),dip3,sz,cifip6z,cisip6z,ciwip6z,cifz6,cisz6,ciwz6,&
         (ph3%zen(1)-ph3%zst(1)+1),(ph3%zen(2)-ph3%zst(2)+1),nzmsize,zsize(3),1)
    !WORK Y-PENCILS
    call transpose_z_to_y(ppi3,pp2,ph3) !nxm nym nz
    call interypv(ppi2,pp2,dip2,sy,cifip6y,cisip6y,ciwip6y,cify6,cisy6,ciwy6,&
         (ph3%yen(1)-ph3%yst(1)+1),nymsize,ysize(2),ysize(3),1)
    !WORK X-PENCILS
    call transpose_y_to_x(ppi2,pp1,ph2) !nxm ny nz
    call interxpv(tmp_pres,pp1,di1,sx,cifip6,cisip6,ciwip6,cifx6,cisx6,ciwx6,&
         nxmsize,xsize(1),xsize(2),xsize(3),1)
    !The pressure field on the main mesh is in tmp_pres
  endif

  if (myflag) iimplicit=1 ! no se para qué usar iimplicit y flag: OK

  if (iscalar.eq.1) then
    call update_user_stats(ux1,uy1,uz1,px1,py1,pz1,&
				dudx,dudy,dudz,& ! Grad ux
				dvdx,dvdy,dvdz,& ! Grad uy
				dwdx,dwdy,dwdz,& ! Grad uz
				dudxx,dudyy,dudzz,dudxy,dudxz,dudyz,&
				dvdxx,dvdyy,dvdzz,dvdxy,dvdxz,dvdyz,&
				dwdxx,dwdyy,dwdzz,dwdxy,dwdxz,dwdyz,&
				tmp_pres,pp3(:,:,:,1),&
				phi1(:,:,:,1),&
				dtdx,dtdy,dtdz,& ! Grad phi
				dtdxx,dtdyy,dtdzz,dtdxy,dtdxz,dtdyz,& ! Laplacian phi
				dtdxxx,dtdxyy,dtdxzz,&
				dtdyxx,dtdyyy,dtdyzz,&
				dtdzxx,dtdzyy,dtdzzz,&
				phG,ph1,ph2,ph3,ph4)
  else
    call update_user_stats(ux1,uy1,uz1,px1,py1,pz1,&
				dudx,dudy,dudz,& ! Grad ux
				dvdx,dvdy,dvdz,& ! Grad uy
				dwdx,dwdy,dwdz,& ! Grad uz
				dudxx,dudyy,dudzz,dudxy,dudxz,dudyz,&
				dvdxx,dvdyy,dvdzz,dvdxy,dvdxz,dvdyz,&
				dwdxx,dwdyy,dwdzz,dwdxy,dwdxz,dwdyz,&
				tmp_pres,pp3(:,:,:,1),&
				phG,ph1,ph2,ph3,ph4)
  endif

end subroutine pre_update_user_stats


! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
subroutine pre_update_user_spat(phGin,ph1in,ph2in,ph3in,ph4in)

  use decomp_2d_io, only : decomp_2d_write_plane
  use decomp_2d, only : xsize, ysize, zsize, DECOMP_INFO, get_decomp_info
  use param, only : istret, iscalar, zero, iimplicit !(para qué usarlo??)
  use variables
  use var

  implicit none

  TYPE(DECOMP_INFO), intent(in) :: phGin,ph1in,ph2in,ph3in,ph4in
  logical :: myflag
  integer :: i,j,k

  ! Pressure interpolation
  real(mytype), dimension(xsize(1),xsize(2),xsize(3)) :: tmp_pres
  character(len=10) :: timer

!  real(mytype),dimension(ysize(1),8,ysize(3)) :: output_tdyt
  real(mytype),dimension(ysize(1),4,ysize(3)) :: output_tdyt

  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dudx, dudy, dudz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dvdx, dvdy, dvdz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dwdx, dwdy, dwdz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dudxx,dudyy,dudzz,dudxy,dudxz,dudyz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dvdxx,dvdyy,dvdzz,dvdxy,dvdxz,dvdyz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dwdxx,dwdyy,dwdzz,dwdxy,dwdxz,dwdyz

  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdx, dtdy, dtdz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdxx,dtdyy,dtdzz,dtdxy,dtdxz,dtdyz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdxxx,dtdxyy,dtdxzz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdyxx,dtdyyy,dtdyzz
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dtdzxx,dtdzyy,dtdzzz
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: dudt, dvdt, dwdt
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

TYPE(DECOMP_INFO) :: phglob

call get_decomp_info(phglob)

  myflag=.false. 
  if (iimplicit.eq.1) myflag=.true.
  if (myflag) iimplicit=0

  ! Compute laplacian temperature (budget uPhi, vPhi, wPhi) and grad(phi)
  if (iscalar.eq.1) then

    ! Compute laplacian phi
    call transpose_x_to_y(phi1(:,:,:,1),phi2(:,:,:,1),phglob)
    call transpose_y_to_z(phi2(:,:,:,1),phi3(:,:,:,1))
    !
    call derx(dtdx,phi1(:,:,:,1),di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero)
    call derxx (dtdxx,phi1(:,:,:,1),di1,sx,sfx,ssx,swx,xsize(1),xsize(2),xsize(3),0,zero)
    call derx(dtdxxx,dtdxx,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero)
    !
    call dery (tb2,phi2(:,:,:,1),di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
    call transpose_y_to_x(tb2,dtdy) ! dTdy
    call derx(dtdxy,dtdy,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero) ! dTdxy
    call derxx (dtdyxx,dtdy,di1,sx,sfx,ssx,swx,xsize(1),xsize(2),xsize(3),0,zero)
    !
    call deryy (ta2,phi2(:,:,:,1),di2,sy,sfy,ssy,swy,ysize(1),ysize(2),ysize(3),0,zero)
    if (istret.ne.0) then 
      do j=1,ysize(2)
        ta2(:,j,:)=ta2(:,j,:)*pp2y(j)-pp4y(j)*tb2(:,j,:) ! dyy dans ta2
      enddo
    endif
    call transpose_y_to_x(ta2,dtdyy) ! dTdyy
    call derx(dtdxyy,dtdyy,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero) ! dTdxyy
    call dery (tc2,ta2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
    call transpose_y_to_x(tc2,dtdyyy) ! dTdyyy
    call transpose_y_to_z(ta2,ta3)
    call derz (tb3,ta3,di3,sz,ffz,fsz,fwz,zsize(1),zsize(2),zsize(3),0,zero)
    call transpose_z_to_y(tb3,ta2)
    call transpose_y_to_x(ta2,dtdzyy)
    !
    call derz (ta3,phi3(:,:,:,1),di3,sz,ffz,fsz,fwz,zsize(1),zsize(2),zsize(3),0,zero)
    call transpose_z_to_y(ta3,ta2) ! dTdz dans tb2
    call transpose_y_to_x(ta2,dtdz) ! dTdz
    call derxx (dtdzxx,dtdz,di1,sx,sfx,ssx,swx,xsize(1),xsize(2),xsize(3),0,zero)
    call dery (tc2,ta2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
    call transpose_y_to_x(tc2,dtdyz) ! dTdyz
    call derx(dtdxz,dtdz,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero)
    call derzz (ta3,phi3(:,:,:,1),di3,sz,sfz,ssz,swz,zsize(1),zsize(2),zsize(3),0,zero)
    call transpose_z_to_y(ta3,ta2) ! dzz dans ta2
    call transpose_y_to_x(ta2,dtdzz) ! dTdzz
    call derx(dtdxzz,dtdzz,di1,sx,ffx,fsx,fwx,xsize(1),xsize(2),xsize(3),0,zero)
    call dery (tc2,ta2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
    call transpose_y_to_x(tc2,dtdyzz)
    call derz (tb3,ta3,di3,sz,ffz,fsz,fwz,zsize(1),zsize(2),zsize(3),0,zero)
    call transpose_z_to_y(tb3,ta2)
    call transpose_y_to_x(ta2,dtdzzz)

  endif

  ! Compute velocity gradient
  call derx (dudx,ux1,di1,sx,ffx ,fsx ,fwx ,xsize(1),xsize(2),xsize(3),0,zero)
  call derx (dvdx,uy1,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dwdx,uz1,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  ! ta1=dudx / tb1=dvdx / tc1=dwdx
  call transpose_x_to_y(ux1,ux2)
  call transpose_x_to_y(uy1,uy2)
  call transpose_x_to_y(uz1,uz2)

! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
! we save in each plane (y pencil) u,v,w and phi2 in y^+=10, y=0.0565
!if (mod(itime,20).eq.0) then
!154 format('t',I9.9)
!  write(timer,154) itime
!  output_tdyt(:,1,:)= ux2(:,11,:)
!  output_tdyt(:,2,:)= uy2(:,11,:)
!  output_tdyt(:,3,:)= uz2(:,11,:)
!  output_tdyt(:,4,:)= phi2(:,11,:)
!  call decomp_2d_write_plane(2,output_tdyt,5,4,'slices/uvwt_flu_'//timer//'.dat')
!endif
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

  call dery (ta2,ux2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call dery (tb2,uy2,di2,sy,ffy,fsy,fwy,ppy,ysize(1),ysize(2),ysize(3),0,zero)
  call dery (tc2,uz2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call transpose_y_to_x(ta2,dudy)!dudy
  call transpose_y_to_x(tb2,dvdy)!dvdy
  call transpose_y_to_x(tc2,dwdy)!dwdy
! ta2=dudy / tb2=dvdy / tc2=dwdy
  call transpose_y_to_z(ux2,ux3)
  call transpose_y_to_z(uy2,uy3)
  call transpose_y_to_z(uz2,uz3)
  call derz (ta3,ux3,di3,sz,ffzp,fszp,fwzp,zsize(1),zsize(2),zsize(3),1,zero)
  call derz (tb3,uy3,di3,sz,ffzp,fszp,fwzp,zsize(1),zsize(2),zsize(3),1,zero)
  call derz (tc3,uz3,di3,sz,ffz ,fsz ,fwz ,zsize(1),zsize(2),zsize(3),0,zero)
  ! ta3=dudz / tb3=dvdz / tc3=dwdz
  ! Back in y
  call transpose_z_to_y(ta3,td2)!dudz
  call transpose_z_to_y(tb3,te2)!dvdz
  call transpose_z_to_y(tc3,tf2)!dwdz
  ! Back to x
  call transpose_y_to_x(td2,dudz)!dudz
  call transpose_y_to_x(te2,dvdz)!dvdz
  call transpose_y_to_x(tf2,dwdz)!dwdz

  ! Compute velocity second derivatives
  call derxx (dudxx,ux1,di1,sx,sfx ,ssx ,swx ,xsize(1),xsize(2),xsize(3),0,zero)
  call derx (dudxy,dudy,di1,sx,ffx ,fsx ,fwx ,xsize(1),xsize(2),xsize(3),0,zero)
  call derx (dudxz,dudz,di1,sx,ffx ,fsx ,fwx ,xsize(1),xsize(2),xsize(3),0,zero)
  call derxx (dvdxx,uy1,di1,sx,sfxp,ssxp,swxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dvdxy,dvdy,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dvdxz,dvdz,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derxx (dwdxx,uz1,di1,sx,sfxp,ssxp,swxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dwdxy,dwdy,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  call derx (dwdxz,dwdz,di1,sx,ffxp,fsxp,fwxp,xsize(1),xsize(2),xsize(3),1,zero)
  !
  call dery (tg2,td2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call transpose_y_to_x(tg2,dudyz)!dudyz
  call dery (tg2,te2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call transpose_y_to_x(tg2,dvdyz)!dvdyz
  call dery (tg2,tf2,di2,sy,ffyp,fsyp,fwyp,ppy,ysize(1),ysize(2),ysize(3),1,zero)
  call transpose_y_to_x(tg2,dwdyz)!dwdyz
  !
  call deryy (td2,ux2,di2,sy,sfyp,ssyp,swyp,ysize(1),ysize(2),ysize(3),1,zero)
  do j=1,ysize(2)
    td2(:,j,:)=td2(:,j,:)*pp2y(j)-pp4y(j)*ta2(:,j,:)
  enddo
  call transpose_y_to_x(td2,dudyy)!dudyy
  call deryy (te2,uy2,di2,sy,sfy,ssy,swy,ysize(1),ysize(2),ysize(3),0,zero)
  do j=1,ysize(2)
    te2(:,j,:)=te2(:,j,:)*pp2y(j)-pp4y(j)*tb2(:,j,:)
  enddo
  call transpose_y_to_x(te2,dvdyy)!dvdyy
  call deryy (tf2,uz2,di2,sy,sfyp,ssyp,swyp,ysize(1),ysize(2),ysize(3),1,zero)
  do j=1,ysize(2)
    tf2(:,j,:)=tf2(:,j,:)*pp2y(j)-pp4y(j)*tc2(:,j,:)
  enddo
  call transpose_y_to_x(tf2,dwdyy)!dwdyy
  !
  call derzz (ta3,ux3,di3,sz,sfzp,sszp,swzp,zsize(1),zsize(2),zsize(3),1,zero)
  call derzz (tb3,uy3,di3,sz,sfzp,sszp,swzp,zsize(1),zsize(2),zsize(3),1,zero)
  call derzz (tc3,uz3,di3,sz,sfz ,ssz ,swz ,zsize(1),zsize(2),zsize(3),0,zero)
  call transpose_z_to_y(ta3,td2)!dudzz
  call transpose_z_to_y(tb3,te2)!dvdzz
  call transpose_z_to_y(tc3,tf2)!dwdzz
  call transpose_y_to_x(td2,dudzz)!dudzz
  call transpose_y_to_x(te2,dvdzz)!dvdzz
  call transpose_y_to_x(tf2,dwdzz)!dwdzz

  if (.true.) then
    !WORK Z-PENCILS
    call interzpv(ppi3,pp3(:,:,:,1),dip3,sz,cifip6z,cisip6z,ciwip6z,cifz6,cisz6,ciwz6,&
         (ph3%zen(1)-ph3%zst(1)+1),(ph3%zen(2)-ph3%zst(2)+1),nzmsize,zsize(3),1)
    !WORK Y-PENCILS
    call transpose_z_to_y(ppi3,pp2,ph3) !nxm nym nz
    call interypv(ppi2,pp2,dip2,sy,cifip6y,cisip6y,ciwip6y,cify6,cisy6,ciwy6,&
         (ph3%yen(1)-ph3%yst(1)+1),nymsize,ysize(2),ysize(3),1)
    !WORK X-PENCILS
    call transpose_y_to_x(ppi2,pp1,ph2) !nxm ny nz
    call interxpv(tmp_pres,pp1,di1,sx,cifip6,cisip6,ciwip6,cifx6,cisx6,ciwx6,&
         nxmsize,xsize(1),xsize(2),xsize(3),1)
    !The pressure field on the main mesh is in tmp_pres
  endif

  if (myflag) iimplicit=1 ! no se para qué usar iimplicit y flag

! xxxxxxxxxxxxxxxxxx
! calculando dudt from this subroutine
! I think it is the same than visududt.f90, por ahi hay algun error de redondeo en dvdt
	dudt(:,:,:) = (dudxx(:,:,:) + dudyy(:,:,:) + dudzz(:,:,:))/re &
         - ux1(:,:,:)*dudx(:,:,:) - uy1(:,:,:)*dudy(:,:,:) - uz1(:,:,:)*dudz(:,:,:) - &
	   px1/gdt(iadvance_time)
	dvdt(:,:,:) = (dvdxx(:,:,:) + dvdyy(:,:,:) + dvdzz(:,:,:))/re &
         - ux1(:,:,:)*dvdx(:,:,:) - uy1(:,:,:)*dvdy(:,:,:) - uz1(:,:,:)*dvdz(:,:,:) - &
	   py1(:,:,:)/gdt(iadvance_time)	
	dwdt(:,:,:) = (dwdxx(:,:,:) + dwdyy(:,:,:) + dwdzz(:,:,:))/re &
         - ux1(:,:,:)*dwdx(:,:,:) - uy1(:,:,:)*dwdy(:,:,:) - uz1(:,:,:)*dwdz(:,:,:) - &
	   pz1(:,:,:)/gdt(iadvance_time)
!	print *, "we are here in dudt mystat:", nrank, gdt(iadvance_time), re	
!    call update_spat(ux1,uy1,uz1,px1,py1,pz1,&
!				dudt,dvdt,dwdt,&

! xxxxxxxxxxxxxxxxxx

  if (iscalar.eq.1) then
    call update_spat(ux1,uy1,uz1,px1,py1,pz1,&
				dudt,dvdt,dwdt,&
				dudx,dudy,dudz,& ! Grad ux
				dvdx,dvdy,dvdz,& ! Grad uy
				dwdx,dwdy,dwdz,& ! Grad uz
				dudxx,dudyy,dudzz,dudxy,dudxz,dudyz,&
				dvdxx,dvdyy,dvdzz,dvdxy,dvdxz,dvdyz,&
				dwdxx,dwdyy,dwdzz,dwdxy,dwdxz,dwdyz,&
				tmp_pres,pp3(:,:,:,1),&
				phi1(:,:,:,1),&
				dtdx,dtdy,dtdz,& ! Grad phi
				dtdxx,dtdyy,dtdzz,dtdxy,dtdxz,dtdyz,& ! Laplacian phi
				dtdxxx,dtdxyy,dtdxzz,&
				dtdyxx,dtdyyy,dtdyzz,&
				dtdzxx,dtdzyy,dtdzzz,&
				phG,ph1,ph2,ph3,ph4)
  else
    call update_spat(ux1,uy1,uz1,px1,py1,pz1,&
				dudt,dvdt,dwdt,&
				dudx,dudy,dudz,& ! Grad ux
				dvdx,dvdy,dvdz,& ! Grad uy
				dwdx,dwdy,dwdz,& ! Grad uz
				dudxx,dudyy,dudzz,dudxy,dudxz,dudyz,&
				dvdxx,dvdyy,dvdzz,dvdxy,dvdxz,dvdyz,&
				dwdxx,dwdyy,dwdzz,dwdxy,dwdxz,dwdyz,&
				tmp_pres,pp3(:,:,:,1),&
				phG,ph1,ph2,ph3,ph4)
  endif

end subroutine pre_update_user_spat
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
subroutine write_user_stats(phG,ph1,ph2,ph3,ph4)

use decomp_2d, only : DECOMP_INFO, get_decomp_info
use decomp_2d_io, only : decomp_2d_write_plane, decomp_2d_write_one
use param, only : iscalar

implicit none

TYPE(DECOMP_INFO), intent(in) :: phG,ph1,ph2,ph3,ph4
TYPE(DECOMP_INFO) :: decomp_main

call decomp_2d_write_one(1,um,'stats/um.dat',decomp_user_stats)
call decomp_2d_write_one(1,vm,'stats/vm.dat',decomp_user_stats)
call decomp_2d_write_one(1,wm,'stats/wm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dpdxm,'dpdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdym,'dpdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdzm,'dpdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudym,'dudym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdym,'dvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdym,'dwdym.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudyym,'dudyym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdyym,'dvdyym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdyym,'dwdyym.dat',decomp_user_stats)

call decomp_2d_write_one(1,uum,'stats/uum.dat',decomp_user_stats)
call decomp_2d_write_one(1,vvm,'stats/vvm.dat',decomp_user_stats)
call decomp_2d_write_one(1,wwm,'stats/wwm.dat',decomp_user_stats)

call decomp_2d_write_one(1,uvm,'stats/uvm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,uwm,'uwm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vwm,'vwm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,udpdxm,'udpdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,udpdym,'udpdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,udpdzm,'udpdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vdpdxm,'vdpdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdpdym,'vdpdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdpdzm,'vdpdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,wdpdxm,'wdpdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,wdpdym,'wdpdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,wdpdzm,'wdpdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,uuum,'uuum.dat',decomp_user_stats)
! call decomp_2d_write_one(1,uvvm,'uvvm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,uwwm,'uwwm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vuum,'vuum.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vvvm,'vvvm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vwwm,'vwwm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,wuum,'wuum.dat',decomp_user_stats)
! call decomp_2d_write_one(1,wvvm,'wvvm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,wwwm,'wwwm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,uvwm,'uvwm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudx2m,'dudx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudy2m,'dudy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudz2m,'dudz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdx2m,'dvdx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdy2m,'dvdy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdz2m,'dvdz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dwdx2m,'dwdx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdy2m,'dwdy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdz2m,'dwdz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudvdxm,'dudvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudwdxm,'dudwdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdwdxm,'dvdwdxm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudvdym,'dudvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudwdym,'dudwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdwdym,'dvdwdym.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudvdzm,'dudvdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudwdzm,'dudwdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdwdzm,'dvdwdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudxdudym,'dudxdudym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudxdudzm,'dudxdudzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudydudzm,'dudydudzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdxdvdym,'dvdxdvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdxdvdzm,'dvdxdvdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdydvdzm,'dvdydvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dwdxdwdym,'dwdxdwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdxdwdzm,'dwdxdwdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdydwdzm,'dwdydwdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudxdvdym,'dudxdvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudydvdxm,'dudydvdxm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudxdvdzm,'dudxdvdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudzdvdxm,'dudzdvdxm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudydvdzm,'dudydvdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudzdvdym,'dudzdvdym.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudxdwdym,'dudxdwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudydwdxm,'dudydwdxm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudxdwdzm,'dudxdwdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudzdwdxm,'dudzdwdxm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudydwdzm,'dudydwdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudzdwdym,'dudzdwdym.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdxdwdym,'dvdxdwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdydwdxm,'dvdydwdxm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdxdwdzm,'dvdxdwdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdzdwdxm,'dvdzdwdxm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdydwdzm,'dvdydwdzm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdzdwdym,'dvdzdwdym.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vdudxm,'vdudxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdudym,'vdudym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdudzm,'vdudzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vdvdxm,'vdvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdvdym,'vdvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdvdzm,'vdvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vdwdxm,'vdwdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdwdym,'vdwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdwdzm,'vdwdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudx_dududxm,'dudx_dududxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudx_dududym,'dudx_dududym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudx_dududzm,'dudx_dududzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudy_dudvdxm,'dudy_dudvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudy_dudvdym,'dudy_dudvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudy_dudvdzm,'dudy_dudvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudz_dudwdxm,'dudz_dudwdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudz_dudwdym,'dudz_dudwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudz_dudwdzm,'dudz_dudwdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdx_dvdudxm,'dvdx_dvdudxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdx_dvdudym,'dvdx_dvdudym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdx_dvdudzm,'dvdx_dvdudzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdy_dvdvdxm,'dvdy_dvdvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdy_dvdvdym,'dvdy_dvdvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdy_dvdvdzm,'dvdy_dvdvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdz_dvdwdxm,'dvdz_dvdwdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdz_dvdwdym,'dvdz_dvdwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdz_dvdwdzm,'dvdz_dvdwdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dwdx_dwdudxm,'dwdx_dwdudxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdx_dwdudym,'dwdx_dwdudym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdx_dwdudzm,'dwdx_dwdudzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dwdy_dwdvdxm,'dwdy_dwdvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdy_dwdvdym,'dwdy_dwdvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdy_dwdvdzm,'dwdy_dwdvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dwdz_dwdwdxm,'dwdz_dwdwdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdz_dwdwdym,'dwdz_dwdwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdz_dwdwdzm,'dwdz_dwdwdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudx_dudvdxm,'dudx_dudvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudx_dudvdym,'dudx_dudvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudx_dudvdzm,'dudx_dudvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudy_dvdvdxm,'dudy_dvdvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudy_dvdvdym,'dudy_dvdvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudy_dvdvdzm,'dudy_dvdvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudz_dvdwdxm,'dudz_dvdwdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudz_dvdwdym,'dudz_dvdwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudz_dvdwdzm,'dudz_dvdwdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdx_dududxm,'dvdx_dududxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdx_dududym,'dvdx_dududym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdx_dududzm,'dvdx_dududzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdy_dudvdxm,'dvdy_dudvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdy_dudvdym,'dvdy_dudvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdy_dudvdzm,'dvdy_dudvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdz_dudwdxm,'dvdz_dudwdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdz_dudwdym,'dvdz_dudwdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdz_dudwdzm,'dvdz_dudwdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vdudx2m,'vdudx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdudy2m,'vdudy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdudz2m,'vdudz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vdvdx2m,'vdvdx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdvdy2m,'vdvdy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdvdz2m,'vdvdz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vdwdx2m,'vdwdx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdwdy2m,'vdwdy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdwdz2m,'vdwdz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,vdudvdxm,'vdudvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdudvdym,'vdudvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,vdudvdzm,'vdudvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dpdvdxm,'dpdvdxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdvdym,'dpdvdym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdvdzm,'dpdvdzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dpdudxm,'dpdudxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdudym,'dpdudym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdudzm,'dpdudzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dpdxdudyxm,'dpdxdudyxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdydudyym,'dpdydudyym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdzdudyzm,'dpdzdudyzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dpdxdvdxxm,'dpdxdvdxxm.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdydvdxym,'dpdydvdxym.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dpdzdvdxzm,'dpdzdvdxzm.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudxx2m,'dudxx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudyy2m,'dudyy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudzz2m,'dudzz2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudxy2m,'dudxy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudxz2m,'dudxz2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudyz2m,'dudyz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dvdxx2m,'dvdxx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdyy2m,'dvdyy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdzz2m,'dvdzz2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdxy2m,'dvdxy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdxz2m,'dvdxz2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dvdyz2m,'dvdyz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dwdxx2m,'dwdxx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdyy2m,'dwdyy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdzz2m,'dwdzz2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdxy2m,'dwdxy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdxz2m,'dwdxz2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dwdyz2m,'dwdyz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudvdxx2m,'dudvdxx2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudvdyy2m,'dudvdyy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudvdzz2m,'dudvdzz2m.dat',decomp_user_stats)

! call decomp_2d_write_one(1,dudvdxy2m,'dudvdxy2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudvdxz2m,'dudvdxz2m.dat',decomp_user_stats)
! call decomp_2d_write_one(1,dudvdyz2m,'dudvdyz2m.dat',decomp_user_stats)


if (iscalar.eq.1) then
  call decomp_2d_write_one(1,phim,'stats/phim.dat',decomp_user_stats)
  !
  ! call decomp_2d_write_one(1,dphidym,'dphidym.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dphidyym,'dphidyym.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dphidyyym,'dphidyyym.dat',decomp_user_stats)
  !
  call decomp_2d_write_one(1,uphim,'stats/uphim.dat',decomp_user_stats)
  call decomp_2d_write_one(1,vphim,'stats/vphim.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,wphim,'wphim.dat',decomp_user_stats)
  call decomp_2d_write_one(1,phiphim,'stats/phiphim.dat',decomp_user_stats)
  !
  ! call decomp_2d_write_one(1,phidpdxm,'phidpdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,phidpdym,'phidpdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,phidpdzm,'phidpdzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dphidx2m,'dphidx2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dphidy2m,'dphidy2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dphidz2m,'dphidz2m.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dudphidxm,'dudphidxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdphidxm,'dvdphidxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dwdphidxm,'dwdphidxm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dudphidym,'dudphidym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdphidym,'dvdphidym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dwdphidym,'dwdphidym.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dudphidzm,'dudphidzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdphidzm,'dvdphidzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dwdphidzm,'dwdphidzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,udtdxxm,'udtdxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,udtdyym,'udtdyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,udtdzzm,'udtdzzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,vdtdxxm,'vdtdxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdtdyym,'vdtdyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdtdzzm,'vdtdzzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,wdtdxxm,'wdtdxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,wdtdyym,'wdtdyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,wdtdzzm,'wdtdzzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,uphi2m,'uphi2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vphi2m,'vphi2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,wphi2m,'wphi2m.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,phiuum,'phiuum.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,phivvm,'phivvm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,phiwwm,'phiwwm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,phiuvm,'phiuvm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,phiuwm,'phiuwm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,phivwm,'phivwm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dtdxdtdym,'dtdxdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdxdtdzm,'dtdxdtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdydtdzm,'dtdydtdzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dtdxdudym,'dtdxdudym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdydudxm,'dtdydudxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdxdudzm,'dtdxdudzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdzdudxm,'dtdzdudxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdydudzm,'dtdydudzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdzdudym,'dtdzdudym.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dtdxdvdym,'dtdxdvdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdydvdxm,'dtdydvdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdxdvdzm,'dtdxdvdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdzdvdxm,'dtdzdvdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdydvdzm,'dtdydvdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdzdvdym,'dtdzdvdym.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dtdxdwdym,'dtdxdwdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdydwdxm,'dtdydwdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdxdwdzm,'dtdxdwdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdzdwdxm,'dtdzdwdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdydwdzm,'dtdydwdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdzdwdym,'dtdzdwdym.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,vdtdxm,'vdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdtdym,'vdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdtdzm,'vdtdzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dudx_dudtdxm,'dudx_dudtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudx_dudtdym,'dudx_dudtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudx_dudtdzm,'dudx_dudtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudy_dvdtdxm,'dudy_dvdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudy_dvdtdym,'dudy_dvdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudy_dvdtdzm,'dudy_dvdtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudz_dwdtdxm,'dudz_dwdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudz_dwdtdym,'dudz_dwdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudz_dwdtdzm,'dudz_dwdtdzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dtdx_dududym,'dtdx_dududym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdx_dududzm,'dtdx_dududzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdy_dudvdxm,'dtdy_dudvdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdy_dudvdzm,'dtdy_dudvdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdz_dudwdxm,'dtdz_dudwdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdz_dudwdym,'dtdz_dudwdym.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dvdx_dudtdxm,'dvdx_dudtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdx_dudtdym,'dvdx_dudtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdx_dudtdzm,'dvdx_dudtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdy_dvdtdxm,'dvdy_dvdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdy_dvdtdym,'dvdy_dvdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdy_dvdtdzm,'dvdy_dvdtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdz_dwdtdxm,'dvdz_dwdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdz_dwdtdym,'dvdz_dwdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdz_dwdtdzm,'dvdz_dwdtdzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dtdx_dudvdym,'dtdx_dudvdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdx_dudvdzm,'dtdx_dudvdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdy_dvdvdxm,'dtdy_dvdvdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdy_dvdvdzm,'dtdy_dvdvdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdz_dvdwdxm,'dtdz_dvdwdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdz_dvdwdym,'dtdz_dvdwdym.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dtdx_dudtdxm,'dtdx_dudtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdx_dudtdym,'dtdx_dudtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdx_dudtdzm,'dtdx_dudtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdy_dvdtdxm,'dtdy_dvdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdy_dvdtdym,'dtdy_dvdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdy_dvdtdzm,'dtdy_dvdtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdz_dwdtdxm,'dtdz_dwdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdz_dwdtdym,'dtdz_dwdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdz_dwdtdzm,'dtdz_dwdtdzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,vdudtdxm,'vdudtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdudtdym,'vdudtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdudtdzm,'vdudtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdvdtdxm,'vdvdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdvdtdym,'vdvdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdvdtdzm,'vdvdtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdtdx2m,'vdtdx2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdtdy2m,'vdtdy2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,vdtdz2m,'vdtdz2m.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dpdtdxm,'dpdtdxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dpdtdym,'dpdtdym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dpdtdzm,'dpdtdzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dpdtdxxm,'dpdtdxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dpdtdxym,'dpdtdxym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dpdtdxzm,'dpdtdxzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dpdtdyxm,'dpdtdyxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dpdtdyym,'dpdtdyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dpdtdyzm,'dpdtdyzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dudtdxxm,'dudtdxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudtdyym,'dudtdyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudtdzzm,'dudtdzzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudtdxym,'dudtdxym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudtdxzm,'dudtdxzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudtdyzm,'dudtdyzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dudx_dtdxxxm,'dudx_dtdxxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudx_dtdyxxm,'dudx_dtdyxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudx_dtdzxxm,'dudx_dtdzxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudy_dtdxyym,'dudy_dtdxyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudy_dtdyyym,'dudy_dtdyyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudy_dtdzyym,'dudy_dtdzyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudz_dtdxzzm,'dudz_dtdxzzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudz_dtdyzzm,'dudz_dtdyzzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dudz_dtdzzzm,'dudz_dtdzzzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dvdtdxxm,'dvdtdxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdtdyym,'dvdtdyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdtdzzm,'dvdtdzzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdtdxym,'dvdtdxym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdtdxzm,'dvdtdxzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdtdyzm,'dvdtdyzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dvdx_dtdxxxm,'dvdx_dtdxxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdx_dtdyxxm,'dvdx_dtdyxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdx_dtdzxxm,'dvdx_dtdzxxm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdy_dtdxyym,'dvdy_dtdxyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdy_dtdyyym,'dvdy_dtdyyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdy_dtdzyym,'dvdy_dtdzyym.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdz_dtdxzzm,'dvdz_dtdxzzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdz_dtdyzzm,'dvdz_dtdyzzm.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dvdz_dtdzzzm,'dvdz_dtdzzzm.dat',decomp_user_stats)
  ! !
  ! call decomp_2d_write_one(1,dtdxx2m,'dtdxx2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdyy2m,'dtdyy2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdzz2m,'dtdzz2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdxy2m,'dtdxy2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdxz2m,'dtdxz2m.dat',decomp_user_stats)
  ! call decomp_2d_write_one(1,dtdyz2m,'dtdyz2m.dat',decomp_user_stats)
  !
endif

! call decomp_2d_write_one(1,presm ,'presm.dat' ,decomp_user_stats)
! call decomp_2d_write_one(1,pres2m,'pres2m.dat',decomp_user_stats)

end subroutine write_user_stats
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

!!!function (var)
!!!end function 
!!!
! There was read statistics here
!!!

end module user_stats
