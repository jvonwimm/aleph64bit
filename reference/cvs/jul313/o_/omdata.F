      SUBROUTINE OMDATA
C----------------------------------------------------------------------
C!  - Fill COMMON from data base for BOM analysis
C!
C!   Author:  R.W.Forty  23-May-91
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "bomflg.h"
#include "bomopt.h"
#include "bomcal.h"
#include "bomojj.h"
#include "bomdjj.h"
#include "bomgjj.h"
#include "bommjj.h"
C
      INTEGER I, J, KAB, IROW, AGETDB, IRET, KBOMD, KBOMG,
     &        KBOMM, KBOMO
#include "bmacro.h"
C.......................................................................
C
C  Find run number
C
      KRUNR = IW( NAMIND('RUNR') )
      IF (KRUNR .EQ. 0) GOTO 998
      IRUN = IW(KRUNR + 2)
C
C  Read banks from data base
C
      IRET = AGETDB ('BOMDBOMGBOMMBOMO', IRUN)
      IF (IRET .EQ. 0) GOTO 998
C
      KBOMD = IW( NAMIND('BOMD') )
      KBOMG = IW( NAMIND('BOMG') )
      KBOMM = IW( NAMIND('BOMM') )
      KBOMO = IW( NAMIND('BOMO') )
C
      IF (KBOMD .EQ. 0 .OR.
     &    KBOMG .EQ. 0 .OR.
     &    KBOMM .EQ. 0 .OR.
     &    KBOMO .EQ. 0     ) GOTO 998
C
C  Fill COMMON with calibration constants from banks
C
      DSCALE = RTABL (KBOMD, 1, JBOMDS)
      CSCALE = RTABL (KBOMD, 1, JBOMCS)
      PCALMM = RTABL (KBOMD, 1, JBOMPC)
      XIPOFF(1) = RTABL (KBOMD, 1, JBOMO1)
      XIPOFF(2) = RTABL (KBOMD, 1, JBOMO2)
      XIPOFF(3) = RTABL (KBOMD, 1, JBOMO3)
      XIPOFF(4) = RTABL (KBOMD, 1, JBOMO4)
C
      DO 200 J = 1, 2
        DO 100 I = 1, 4
          IROW = J*4 + I - 4
          GAINA0(I,J) = RTABL (KBOMG, IROW, JBOMG0)
          GAINA1(I,J) = RTABL (KBOMG, IROW, JBOMG1)
          GAINA2(I,J) = RTABL (KBOMG, IROW, JBOMG2)
          CALCOR(I,J) = RTABL (KBOMM, IROW, JBOMMC)
          IADCOR(I,J) = ITABL (KBOMM, IROW, JBOMMA)
          IXYCOR(I)   = ITABL (KBOMM, IROW, JBOMMX)
  100   CONTINUE
  200 CONTINUE
C
C  Fill COMMON with beam optics from banks
C
      DO 500 KAB = 1, 2
        DO 400 J = 1, 4
          DO 300 I = 1, 4
            IROW = KAB*16 + J*4 + I - 20
            RIBOPT(I,J,KAB)   = RTABL (KBOMO, IROW, JBOMIR)
            RBIOPT(I,J,KAB)   = RTABL (KBOMO, IROW, JBOMRI)
            RCBOPT(I,J,KAB,1) = RTABL (KBOMO, IROW, JBOMB1)
            RCBOPT(I,J,KAB,2) = RTABL (KBOMO, IROW, JBOMB2)
            RCBOPT(I,J,KAB,3) = RTABL (KBOMO, IROW, JBOMB3)
            RCBOPT(I,J,KAB,4) = RTABL (KBOMO, IROW, JBOMB4)
            RCIOPT(I,J,KAB,1) = RTABL (KBOMO, IROW, JBOMI1)
            RCIOPT(I,J,KAB,2) = RTABL (KBOMO, IROW, JBOMI2)
            RCIOPT(I,J,KAB,3) = RTABL (KBOMO, IROW, JBOMI3)
            RCIOPT(I,J,KAB,4) = RTABL (KBOMO, IROW, JBOMI4)
  300     CONTINUE
  400   CONTINUE
  500 CONTINUE
C
      XINIFL = .TRUE.
      GOTO 999
C
  998 CALL RERROR ('OMDATA', 1, ' Error reading database')
      XINIFL = .FALSE.
C
  999 RETURN
      END
#endif
