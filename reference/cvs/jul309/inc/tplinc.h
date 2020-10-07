      PARAMETER (LMXZCR=200)
      COMMON/TPLINC/ZCIRCL(LMXZCR),SCIRCL(LMXZCR),WLINEA(LMXZCR)
#if defined(DOC)
C
C!  Work arrays for line fitter
C
C      ZCIRCL = Z position of each point to be fit
C      SCIRCL = Radial position of each point to be fit
C
C--------------------------------------------------------------------
#endif
