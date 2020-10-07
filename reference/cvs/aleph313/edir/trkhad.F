        SUBROUTINE TRKHAD (NGDTR,ECHRG)
C----------------------------------------------------------------------
CKEY EDIR TRACKS ENERGY
C! Returns number of good tracks and charged energy.
C! Good tracks:
C!      |D0| < 2cm, |Z0| < 10cm, >=4 TPC coord., |COS(THETA)|<0.95
C-
C   Input  : None
C   Output : NGDTR  = Number of good charged tracks
C            ECHRG  = Energy sum of NGDTR charged track
C-
C   Called by   : SELCAL,SELTRK
C   Calls  : QPTRCK
C   Input banks : PFRF,PFRT
C-
C                                   Author: Ed. Blucher     18-Oct-1989
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pfrfjj.h"
#include "pfrtjj.h"
C --
      DATA AMPI /0.13956755/
      DATA D0CUT,Z0CUT,NTPCO,ANGCUT/2.,10.,4,.95/
C --
#include "bmacro.h"
C --
      NGDTR = 0
      ECHRG = 0.
C --
      KPFRF = IW(NAMIND('PFRF'))
      KPFRT = IW(NAMIND('PFRT'))
      IF(KPFRF.LE.0 .OR. KPFRT.LE.0) GOTO 999
C --
C   Loop over the charged tracks and selected good candidates
C --
      NTRK = LROWS(KPFRF)
      DO 10 IT = 1,NTRK
        D0 = ABS(RTABL(KPFRF,IT,JPFRD0))
        Z0 = ABS(RTABL(KPFRF,IT,JPFRZ0))
        NH = ITABL(KPFRT,IT,JPFRNT)
        IF(NH.LT.NTPCO .OR. D0.GT.D0CUT .OR. Z0.GT.Z0CUT) GOTO 10
        CALL QPTRCK(IT,PZ,PTOT)
        IF(PTOT.EQ.0.) GOTO 10
        IF(ABS(PZ/PTOT).GT.ANGCUT) GOTO 10
        ECHRG=ECHRG + SQRT(PTOT**2 + AMPI**2)
        NGDTR= NGDTR + 1
   10 CONTINUE
999   RETURN
      END
#endif
