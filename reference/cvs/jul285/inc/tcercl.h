      PARAMETER (MXPTER=4)
      COMMON/TCERCL/ XBRPER(MXPTER),CFRPER(3,MXPTER),
     &               XZRPER(MXPTER),CFZDER(3,MXPTER),
     &               NPRPER,NPZDER
C
#if defined(DOC)
C! Parameterization of the TPC coordinate resolution
C
C NPRPER      =        Number of regions in r*phi
C NPZDER      =        Number of regions in z
C XBRPER(4)   =        XRp from the bank TRSC
C CFRPER(3,4) =        BRp from TRSC
C XZRPER(4)   =        XZ  from TRSC
C CFZDER(3,4) =        BZ  from TRSC
C-----------------------------------------------------------------------
#endif
