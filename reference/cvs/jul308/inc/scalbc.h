      PARAMETER (MJWSC=72, MXTSC=100, MCRSC=6)
      COMMON /SCALBC/JWIRSC(MJWSC),TOFFSC,MPOISC,TSTASC,
     +               TDELSC,TSPASC(MXTSC),TCRASC(MCRSC),TCHASC
      INTEGER JWIRSC,MPOISC
      REAL TOFFSC,TSTASC,TDELSC,TSPASC,TCRASC,TCHASC
#if defined(DOC)
C
C!         SATR calibration constants
C
C MJWSC    : dimension of wire on / wire off bit pattern
C MXTSC    : maximum number of TDC space-drifttime-points
C MCRSC    : number of TDC time correction constants for crates
C JWIRSCi  : bit patterns for wire hv on/off
C TOFFSC   : time offset: drift time = time offset - measured time
C MPOISC   : actual number of points for TDC t-x-relation
C TSTASC   : abszissa of first (t-x)-point
C TDELSC   : difference of abszissas of two (t-x)-points
C TSPASCi  : points for t-x-relation of TDC's
C TCRASCi  : TDC time correction for crate i
C TCHASC   : TDC time correction for channels connected to single wire
#endif
