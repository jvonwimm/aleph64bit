        SUBROUTINE TRKWWS (NGDTP,NGDTN,ECHRG)
C----------------------------------------------------------------------
CKEY EDIR TRACKS ENERGY
C! Returns number of good positive and negative tracks and charged energy.
C! Good tracks:
C!      |D0| < 2cm, |Z0| < 10cm, >=4 TPC coord.
C-
C   Input  : None
C   Output : NGDTP  = Number of good charged positive tracks
C            NGDTN  = Number of good charged negative tracks
C            ECHRG  = Energy sum of NGDTP+NGDTN charged track
C-
C   Called by   : SELWWA,SELWWB
C   Calls  : QPTRCK,QTRKCH
C   Input banks : PFRF,PFRT
C-
C                                   Author: J. Yamartino   28-May-1996
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pfrfjj.h"
#include "pfrtjj.h"
C --
      INTEGER NGDTP,NGDTN,ICHG
      EXTERNAL NAMIND
      DATA AMPI /0.13956755/
      DATA D0CUT,Z0CUT,NTPCO/2.,10.,4/
      DATA NAPFRF, NAPFRT / 0,0 /
C --
#include "bmacro.h"
C --

C ??  How to define new variables.

      NGDTP = 0
      NGDTN = 0
      ECHRG = 0.
C --
      IF( NAPFRF .EQ. 0 ) NAPFRF = NAMIND( 'PFRF' )
      IF( NAPFRT .EQ. 0 ) NAPFRT = NAMIND( 'PFRT' )
      KPFRF = IW(NAPFRF)
      KPFRT = IW(NAPFRT)
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
        CALL QTRKCH(IT,ICHG)
        IF(ICHG.EQ.0.) GOTO 10
        ECHRG=ECHRG + SQRT(PTOT**2 + AMPI**2)
        IF(ICHG.EQ. 1) NGDTP = NGDTP + 1
        IF(ICHG.EQ.-1) NGDTN = NGDTN + 1
   10 CONTINUE
999   RETURN
      END
#endif
