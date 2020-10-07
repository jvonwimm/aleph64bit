      SUBROUTINE QWFILE
CKEY WRITE FILE /USER
C----------------------------------------------------------------------
C! print input / output files
C called from user routines
C                                                   H.Albrecht 06.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C----------------------------------------------------------------------
C
      LU = KUPRNT
   10 DO 100 NU=1,2
        IF (LU .EQ. 0)  GO TO 100
C
        WRITE (LU,1001)  KNEVT,KNEFIL,KNREIN
C
        IF (KNREOU .GT. 0)  THEN
          IF (KFEOUT .LT. 0)  WRITE (LU,1003)
          WRITE (LU,1002)  KNEOUT,KNREOU,KLROUT,KLEOUT
        ELSE
          IF (KFEOUT .LT. 0)  THEN
            WRITE (LU,1003)
          ELSE
            WRITE (LU,1004)
          ENDIF
        ENDIF
C
        IF (XHISTO)  THEN
          WRITE (LU,1005)  CQFHIS
        ELSE
          WRITE (LU,1006)
        ENDIF
C
  100 LU = KUPTER
C
 1001 FORMAT ('0_QWFILE_ Input / output files'/
     +        ' Selected events :',I11/
     +        ' Current input file :',I8,' events '/
     +        21X,'=',I7,' run/event/other records')

 1002 FORMAT (' Event output :',I14,' events'/
     +        21X,'=',I7,' run/event/other records'/
     +        ' Last output event : run / event =',2I7)
 1003 FORMAT (' No FILO card : No event output written')
 1004 FORMAT (' Output file empty')
 1005 FORMAT (' Histogram file : ',A)
 1006 FORMAT (' No histogram output requested')
      END
#endif
