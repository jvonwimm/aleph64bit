      INTEGER FUNCTION INVALID(IROW,ICOL)
C---------------------------------------------------------------------
C! Check the validity of the row and the column number of a storey.
C
C  Patrick Janot -- 30 Sept 1992
C---------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "qcde.h"
C
        invalid = 0
C
        IF ( irow .LE. 0 .OR. irow .GE. 229 ) GOTO 998
C
        IF ( icol .LE. 0 ) GOTO 998
C
        IF     ( irow .LE.   8 .OR. 229-irow .LE.   8 ) THEN
          IF ( icol .GT.  96 ) GOTO 998
C
        ELSEIF ( irow .LE.  24 .OR. 229-irow .LE.  24 ) THEN
          IF ( icol .GT. 192 ) GOTO 998
C
        ELSEIF ( irow .LE.  40 .OR. 229-irow .LE.  40 ) THEN
          IF ( icol .GT. 288 ) GOTO 998
C
        ELSEIF ( icol .GT. 384 ) THEN
          GOTO 998
C
        ELSE
C
        ENDIF
C
        invalid = 1
        GOTO 999
C
  998 IF ( idbg .GE. 1 ) THEN
        WRITE (IW(6),*) 'Run ',krun,' Event ',kevt
        WRITE (IW(6),*) 'Row/Column : ',irow,icol,' are invalid ! '
        CALL looses('INVALIDE',1)
      ENDIF
C
  999 RETURN
      END
#endif
