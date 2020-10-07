      SUBROUTINE TSAWTK(NWIR,RWIRE,BUCK,NTCAN,ITIND)
C--------------------------------------------------------
C!  Look for track candidates in bucket # vs wire
C  position projection using a histogramming method.
C--------------------------------------------------------
#ifndef DOC
C
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpccon.h"
      DIMENSION RWIRE(*),BUCK(*)
      DIMENSION ITIND(*)
C
      PARAMETER (NBIN=100)
      DIMENSION Y(NBIN)
C
      PARAMETER (MXNPK = 3)
      DIMENSION IPF(MXNPK),NPB(MXNPK),PSUM(MXNPK)
C
      DIMENSION IB(100)
C
C  Threshold, max. width and min. no of entries for peak finder
      DATA THRSH/4.0/, MXWID/4/, NPMIN/5/
C
      NTCAN = 0
      IF(NWIR.EQ.0) GO TO 999
      BMAX = ZTPCMX/TVDRFT(1)
      XBIN = 0.12/FLOAT(NBIN)
      SMIN = 0.076
      CALL VZERO(Y,NBIN)
      CALL VZERO(IB,NWIR)
      CALL VZERO(ITIND,NWIR)
C
      DO 20 I=1,NWIR
         SLOPE = RWIRE(I)/(BMAX - BUCK(I)) - SMIN
         IB(I) = SLOPE/XBIN + 1.0
         IF (IB(I).LT.1) IB(I)=1
         IF (IB(I).GT.NBIN) IB(I)=NBIN
         Y(IB(I)) = Y(IB(I)) + 1.
   20 CONTINUE
C
C  Now look for peaks in slope histogram
C
C     NPEAK = no of peaks found
C     IPF(i),NPB(i) = offset and number of bins in ith peak
C     PSUM(i) = No of entries in ith peak
C
      CALL TWPEAK(Y,NBIN,THRSH,MXWID,IPF,NPB,PSUM,NPEAK,MXNPK)
      DO 30 I=1,NPEAK
         IF(NINT(PSUM(I)).LT.NPMIN) GO TO 30
         NTCAN = NTCAN + 1
         DO 40 IW=1,NWIR
              IF(IB(IW).GT.IPF(I).AND.IB(IW).LE.IPF(I)+NPB(I))
     &               ITIND(IW) = NTCAN
   40    CONTINUE
   30 CONTINUE
  999 RETURN
      END
#endif
