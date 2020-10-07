      PARAMETER (LMXCIR=200)
      COMMON/TPCIRC/NCIRCL,XCIRCL(LMXCIR),YCIRCL(LMXCIR),
     1    WCIRCL(LMXCIR),SIGCIR(LMXCIR)
      REAL               XCIRCL    ,YCIRCL    ,WCIRCL    ,SIGCIR
#if defined(DOC)
C
C!  Work arrays for track-finding circle fitter
C
C     NCIRCL = Number of points to fit
C     XCIRCL = Cartesian x coordinates of the points
C     YCIRCL = Cartesian y coordinates of the points
C     WCIRCL = Weight for each point in fit
C     SIGCIR = Error estimate for each point in fit
C--------------------------------------------------------------------
#endif
