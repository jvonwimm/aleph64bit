      INTEGER JLAYID,JCOFID
      REAL TOFFID,DCOFID
      PARAMETER (JLAYID=8,JCOFID=3)
      COMMON/IDTCCC/TOFFID(JLAYID),DCOFID(JCOFID,JLAYID)
#if defined(DOC)
C ITC Drift-time relation (from IDTC bank)
C
C JLAYID = Dimension (number of ITC layers)
C JCOFID = Dimension (Number of polynomial coeffs.)
C
C TOFFID(i)   = Time offset (ns.) for layer i
C DCOFID(j,i) = Polynomial coeffs. (j=1,2,3) for layer i
C
#endif
