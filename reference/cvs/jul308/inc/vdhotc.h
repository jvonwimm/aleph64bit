      INTEGER LVDL,NWAFN,NSLOM,NVIEWS
      PARAMETER (LVDL=2,NSLOM=15,NWAFN=4)
      PARAMETER (NVIEWS=2)
      INTEGER IHOTFR,IHOTLA
      COMMON/VDHOTC/IHOTFR(LVDL,NWAFN,NSLOM,NVIEWS),
     &              IHOTLA(LVDL,NWAFN,NSLOM,NVIEWS)