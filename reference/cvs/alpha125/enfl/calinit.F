      SUBROUTINE CALINIT(KEEP)
C----------------------------------------------------------------------
C! Initialize EBNEUT, BOS banks, etc...
C
C  Patrick Janot -- 18 Apr 1990
C----------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
      DIMENSION jer(10)
      LOGICAL first
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
      DATA nmsg/0/
      DATA first/.TRUE./
#include "qmacro.h"
C----------------------------------------------------------------------
      keep = 0
      IF ( idbg .GE. 1 ) CALL looses('CALINIT ',1)
C
C 1-- Bos
C
      CALL bkini(jer)
      IF ( jer(10) .NE. 0 ) THEN
        nmsg = nmsg + 1
        IF ( nmsg .LE. 100 ) THEN
          IF (idbg .GE. 1 ) WRITE (IW(6),1001) jer,krun,kevt
        ELSEIF ( nmsg .EQ. 101 ) THEN
          IF (idbg .GE. 1 ) WRITE (IW(6),*)
     .          '+++ BKINI +++ Maximum nb of msg reached'
        ENDIF
        GOTO 999
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('CALINIT ',2)
C
C 2-- Too many clusters (--> fake event)
C
      IF ( npeco .GT. 200 .OR. nphco .GT. 200 ) THEN
        IF (idbg .GE. 1 ) WRITE (IW(6),1002) npeco,nphco,krun,kevt
        GOTO 999
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('CALINIT ',3)
C
C 4-- Good modules
C
      CALL sicamod
      CALL lumimod
      CALL goodmod(keep)
      IF ( keep .EQ. 0 ) GOTO 999
      IF ( idbg .GE. 1 ) CALL looses('CALINIT ',4)
C
C 5-- Initialize HCAL cleaning
C
      CALL quphco(ier)
      IF ( ier .NE. 0 ) GOTO 999
      IF ( idbg .GE. 1 ) CALL looses('CALINIT ',5)
C
      CALL qhmost(ier)
C
      keep = 1
C
  999 RETURN
C-----------------------------------------------------------------------
 1001 FORMAT(1x,'+++ Error in BKINI code ',10I2,
     .          ' +++ Run ',I9,' Evt ',I9)
 1002 FORMAT(1x,'+++ Too many clusters : ',2(I3,1X),
     .         ' +++ Run ',I9,' Evt ',I9)
      END
#endif
