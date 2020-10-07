*CD fmgeom
      PARAMETER (LNRG=11)
      COMMON / FMGEOM / FMZMIN(LNRG),FMZMAX(LNRG),FMRMIN(LNRG)
     1                  ,FMRMAX(LNRG)
#if defined(DOC)
      Limits of field regions along Z and R axes.To be used in
      conjunction with GUFLD routine.
      FMZMIN   = Z minimum of region
      FMZMAX   = Z maximum of region
      FMRMIN   = R minimum of region
      FMRMAX   = R maximum of region
#endif
