C!    VDET common for bonding errors, peculiar channels
C ----------------------------------------------------------
C bonding errors
      INTEGER MAXERR
      PARAMETER(MAXERR=100)
      INTEGER MXMOD
      PARAMETER(MXMOD = 48)
      INTEGER NUMERR, IBNERR
      INTEGER LSVPCH, LVVPCH, LFVPCH
C
      COMMON/VBNDER/NUMERR(MXMOD,2),IBNERR(MXMOD,2,MAXERR,5),
     > LSVPCH(MXMOD,MAXERR),LVVPCH(MXMOD,MAXERR),
     >     LFVPCH(MXMOD,MAXERR)
#if defined(DOC)
*      NUMERR(MXMOD,2) number of errors by global module,side
*      IBNERR(MXMOD,2,MAXERR,5) error list by global mod,side
*        ibnerr(...,1) = first channel address
*        ibnerr(...,2) = last channel address
*        ibnerr(...,3) = bond number
*        ibnerr(...,4) = fault type
*        ibnerr(...,5) = parameter
#endif
