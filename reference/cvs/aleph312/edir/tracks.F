        SUBROUTINE TRACKS (NTRK,CLAS5,CLAS6)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS5->6
C! Tracks and TPC hits, classes 5 and 6 code.
C-
C   Input  : None
C   Output : NTRK  = Number of TPC tracks with Nhits > 4, |D0| <  5 cm
C                                                     and |Z0| < 20 cm
C            CLAS5 = Class 5 logical flag
C            CLAS6 = Class 6 logical flag
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : PFRF,PFRT
C-
C                                        Author: M. Talby September 89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pfrfjj.h"
#include "pfrtjj.h"
C --
      LOGICAL CLAS5,CLAS6
C --
#include "bmacro.h"
C --
      NTRK = 0
      CLAS5 = .FALSE.
      CLAS6 = .FALSE.
C --
      KPFRF = IW(NAMIND('PFRF'))
      KPFRT = IW(NAMIND('PFRT'))
      IF(KPFRF.LE.0 .OR. KPFRT.LE.0) GOTO 999
C --
C   Loop on charged tracks
C --
      NTRACK = LROWS(KPFRF)
      DO 10 ITK = 1,NTRACK
        D0 = ABS(RTABL(KPFRF,ITK,JPFRD0))
        Z0 = ABS(RTABL(KPFRF,ITK,JPFRZ0))
        NH = ITABL(KPFRT,ITK,JPFRNT)
        IF(NH.LT.4 .OR. D0.GT.5. .OR. Z0 .GT.20.) GO TO 10
        NTRK = NTRK+1
   10 CONTINUE
C --
      IF(NTRK.GE.1 .AND. NTRK.LE.7) CLAS5 = .TRUE.
      IF(NTRK.GE.8) CLAS6 = .TRUE.
C --
  999 RETURN
      END
#endif
