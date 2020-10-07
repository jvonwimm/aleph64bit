*----- inclpd.f ---------------------------------------------------
      parameter   ( MPROC  =  76 )
*     parameter   ( NGRAPH = 144 )
      integer      ngd(0:MPROC)
      character*16 kmpr_c(0:MPROC,6)
      integer      kmpr_l(0:MPROC,6)
      integer      amas_1(0:MPROC,6)
      integer      kcha_g(0:MPROC,6)
      integer      kfco_e(0:MPROC,6)
      real*8       cf_mtx(0:MPROC,0:1,0:1)
      integer      kmcb_s(0:MPROC)
      integer      kmcb_x(0:MPROC)
      integer      kmcs_r(0:MPROC,4,0:1)
      integer      kdiagr(0:MPROC,20,NGRAPH)

      common/cprdbi/ngd,kmpr_l,amas_1,kcha_g,kfco_e,
     &              kmcb_s,kmcb_x,kmcs_r,kdiagr
      common/cprdbd/cf_mtx
      common/cprdbc/kmpr_c
