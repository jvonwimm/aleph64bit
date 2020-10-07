        SUBROUTINE GGTRKH (NGDTR,ECHRG)
C----------------------------------------------------------------------
CKEY EDIR TRACKS ENERGY GAMMA_GAMMA
C! Returns number of good tracks and charged energy.
C! Good tracks:
C!      |D0| < 25cm, |Z0| < 100cm, >=4 TPC coord., |COS(THETA)|<0.97
C!      |P| > 0.1
C-
C   Input  : None
C   Output : NGDTR  = Number of good charged tracks
C            ECHRG  = Energy sum of NGDTR charged track
C-
C   Called by   : SELGGE
C   Calls  : QPTRCK
C   Input banks : PFRF,PFRT
C-
C                                   Author: A.J.Finch       22-May-1996
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pfrfjj.h"
#include "pfrtjj.h"
C --
      EXTERNAL NAMIND
      DATA AMPI /0.13956755/
      DATA D0CUT,Z0CUT,NTPCO,ANGCUT,PCUT/25.,100.,4,.97,0.1/
      DATA NAPFRF, NAPFRT / 0, 0 / 
C --
#include "bmacro.h"
C --
      NGDTR = 0
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
        IF(PTOT.LT.PCUT)GOTO 10
        IF(ABS(PZ/PTOT).GT.ANGCUT) GOTO 10
C extra cut for badly measured tracks with spuriously high energy
        IF(PTOT.GT.15.0.AND.(NH.LT.8.OR.ABS(PZ/PTOT).GT.0.9))GOTO 10
        ECHRG=ECHRG + SQRT(PTOT**2 + AMPI**2)
        NGDTR= NGDTR + 1
   10 CONTINUE
999   RETURN
      END
#endif

