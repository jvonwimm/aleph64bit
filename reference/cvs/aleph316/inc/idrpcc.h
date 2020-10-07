      INTEGER MLAYID,MCOFID,MSPLID,IFLGID
      REAL TTOFID,DTCOID,DSPLID
      PARAMETER (MLAYID=8,MCOFID=5,MSPLID=5)
      COMMON/IDRPCC/TTOFID(MLAYID),DTCOID(MCOFID,MLAYID),
     +              DSPLID(MSPLID,MLAYID),IFLGID
#if defined(DOC)
C Modified 98/08/16 J.Sedgbeer Add DSPLID and IFLGID to end this COMMON.
C
C ITC Drift-time relation from IDTC,IDRP and IDSP banks.
C
C MLAYID = Dimension (number of ITC layers)
C MCOFID = Dimension (Max. Number of polynomial coeffs.)
C MSPLID = Dimension (Max. Number of spline coeffs.)
C
C IFLGID = Integer flag:  IFLGID = 0  - use polynomial in DTCOID(j,i)
C                                       NB. contents of DSPLID undefined
C                                = 1  - use spline in DSPLID(j,i)
C                                       NB. contents of DTCOID undefined
C
C TTOFID(i)   = Time offset (ns.) for layer i
C DTCOID(j,i) = Polynomial coeffs. (j=1,2,...) for layer i (from IDTC or IDRP)
C DSPLID(j,i) = Spline coeffs. (j=1,2,...) for layer i (from IDSP bank)
C
#endif
