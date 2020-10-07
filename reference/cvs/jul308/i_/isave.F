      SUBROUTINE ISAVE(NLINK,LLINK,MODE)
C-----------------------------------------------------------------------
C! Save path in tree.
C!
C!    Author      :- J. Sedgbeer 88/04/14
C!    Modified    :- I. Tomalin  89/06/07
C!
C!   Input:
C!     NLINK    /I : No. of links in path
C!     LLINK(i) /I : List of links in path i=1,NLINK
C!
C!   Output:
C!      commons:     /IPATHC/
C!
C!   called by : ICLIMB
C!   calls     : none
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
C-----------------------------------------------------------------------
C I/O commons
#include "ipathc.h"
      INTEGER LLINK(*)
C-----------------------------------------------------------------------
C
C If first path found (in this tree) then set LPTHIP
C
      IF(NPTHIP.EQ.0) LPTHIP = NLINK
C
C If path shorter than present maximum do not save it
C If path longer  than present maximum then reset LPTHIP and overwrite
C                                                   all previous paths
C If path same length as present maximum then add it to the saved paths
C
      IF (NLINK.LT.LPTHIP) THEN
        GOTO 999
      ELSE IF (NLINK.GT.LPTHIP) THEN
        LPTHIP  = NLINK
        NPTHIP = 1
C Reset error flag as old error was irrelevant.
        IESAVE = 0
      ELSE
C If max. no. of paths already found then set error flag and return
        IF(NPTHIP.GE.MXPTIP) THEN
          IESAVE = 1
          GOTO 999
        ENDIF
        NPTHIP = NPTHIP + 1
      ENDIF
C
C Save path into /IPATHC/
C
      DO 10 I=1,NLINK
        LLNKIP(I,NPTHIP) = LLINK(I)
   10 CONTINUE
C
  999 CONTINUE
      END
#endif
