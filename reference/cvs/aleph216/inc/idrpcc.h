      INTEGER MLAYID,MCOFID
      REAL TTOFID,DTCOID
      PARAMETER (MLAYID=8,MCOFID=5)
      COMMON/IDRPCC/TTOFID(MLAYID),DTCOID(MCOFID,MLAYID)
#if defined(DOC)
C ITC Drift-time relation (from IDRP bank).
C
C MLAYID = Dimension (number of ITC layers)
C MCOFID = Dimension (Max. Number of polynomial coeffs.)
C
C TTOFID(i)   = Time offset (ns.) for layer i
C DTCOID(j,i) = Polynomial coeffs. (j=1,2,...) for layer i
C
#endif
