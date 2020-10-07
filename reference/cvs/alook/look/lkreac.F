      SUBROUTINE LKREAC
C -------------------------------------------------------
C! read input file until 1st event of 1st run or of run
C  required on SRUN card is  found.
C  called at start of file
CKEY LOOK RUNH CLIST
C -------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
#include "lkunpk.h"
      DATA NSLOW/ 0/
C ------------------------------------------------------
      IF (NSLOW.EQ.0) NSLOW=NAMIND('SLOW')
C           READ till the begin of run or 1st event
      CALL BDROP(IW,'E')
      CALL BDROP(IW,'C')
      CALL BLIST(IW,'E=','0')
      CALL BLIST(IW,'C=','0')
      IF (LINSEL.GT.0) CALL BDROP (IW,'$PTS')
      CALL ABMODE ('BATC')
C
 10   IRET = 0
      CALL ABRREC ('E',ULIST,IRET)
      IF (IRET.GE.5) THEN
         WRITE (LOUT,*) ' [EOF or data file not opened]'
         goto 999
      ENDIF
C     reset ULIST
      CALL LKRSUNP
      CALL LKSHOW (IRET)
      IF (IRET.EQ.3) THEN
        CALL BLIST (IW,'T+','E')
        CALL BLIST (IW,'E=','0')
      ENDIF
      IF (IW(NSLOW).EQ.0 .AND. IRET.NE.1) GOTO 10
C
 999  CONTINUE
      CALL ABMODE ('INTE')
      END
#endif
