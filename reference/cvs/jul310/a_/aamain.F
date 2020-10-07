      PROGRAM AAMAIN
C--------------------------------------------------------------------
C! Top Level Reconstruction Program Routine
C!
C!  Author       J. Bunn
C!
C!  Modified     S. Orteu      25-FEB-1987
C!  Modified     O.Callot      14-SEP-1998
C!           Change TIMEST value to get better timing resolution (was 99999999)
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rtimes.h"
#include "version.h"
#include "rjulver.h"
      CHARACTER FNAME*120,FTYPE*8,FDEVI*80
      PARAMETER (LQBOS=3000000)
      COMMON /BCS/IW(LQBOS)
      PARAMETER (MXBNK=3000)
C--------------------------------------------------------------------
C-- This is Stupid!
      RJVERS = CORVER
C
C?   Get Job Starting Time Counter
C
      CALL TIMEST(99999.)
      CALL ALTIME(AJBSRT)
C
C?   Initialise BOS
C
      CALL BNAMES(MXBNK)
      CALL BOS(IW,LQBOS)
C
C set the logical unit for BOS command card read
C
      LCOMRL = 99
C
      FNAME = ' '
      CALL GETENVF ('JULIACARDS',FNAME)
      IF (FNAME.NE.' ') CALL AOPEN(LCOMRL,FNAME,'CARD','DISK',IER)
C
C?   Initialise the job
C
      CALL RINJOB(ISJOB)
      IF (ISJOB.NE.0)                      GOTO 900
C
C?   Process the files
C
      CALL RLOOPR
C
C?   Close the job
C
      CALL RCLJOB
C
      GOTO 999
C--------------------------------------------------------------------
  900 WRITE(LOUTRL,901)
  901 FORMAT(1X,'ABMAIN --> Bad initialisation ... close job')
  999 CONTINUE
      END
#endif
