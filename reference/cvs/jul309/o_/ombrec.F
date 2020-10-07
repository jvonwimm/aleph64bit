      SUBROUTINE OMBREC
C----------------------------------------------------------------------
C!  - Calculate beam parameters using BOM data
C!
C!   Author:  R.W.Forty  23-May-91
C?
C!======================================================================
#ifndef DOC
#include "bomflg.h"
#include "bomcal.h"
C
      REAL DAT(8), SIG(8), PEDS(8), CALI(8), GAIN(4), XYBOM(4),
     &     XIP(4), XIPB(4,2), BCUR(2), BCURB(2,2), GRAT(4), GRATB(4,2)
      INTEGER I, IDPA, IDBU, IRET, IER, IBUNB(2)
C
      DATA XIPB, BCURB, GRATB /8*0., 4*0., 8*0./
      DATA IBUNB /2*0/
C.......................................................................
C
      IF (.NOT. XINIFL) CALL OINIRU
      IF (.NOT. XINIFL) THEN
        IER = -1
        GOTO 500
      ENDIF
C
      IF (.NOT. XCURFL) THEN
        IER = -2
        GOTO 500
      ENDIF
C
C  Calculate corrector deflections
C
      CALL OMCORR
C
C  Loop over e+ and e- beams
C
      DO 400 IDPA = 1, 2
C
C  Read data from banks
C
        CALL OMREAD (IDPA, IDBU, DAT, SIG, PEDS, CALI, IRET)
        IF (IRET .NE. 0) THEN
          IER = -3
          GOTO 500
        ENDIF
C
C  Use beam calibration
C
        CALL OMGAIN (IDPA, DAT, PEDS, CALI, GAIN, GRAT, IRET)
        IF (IRET .NE. 0) THEN
          IER = -4
          GOTO 500
        ENDIF
C
C  Calculate beam position at BOMs
C
        CALL OMBPOS (DAT, PEDS, GAIN, XYBOM, BCUR, IRET)
        IF (IRET .NE. 0) THEN
          IER = -5
          GOTO 500
        ENDIF
C
C  Calculate beam position and angle at IP
C
        CALL OMIPOS (IDPA, XYBOM, XIP, IRET)
        IF (IRET .NE. 0) THEN
          IER = -6
          GOTO 500
        ENDIF
C
        IBUNB(IDPA) = IDBU
C
        DO 200 I = 1, 4
          XIPB (I, IDPA) =  XIP(I) - XIPOFF(I)
          GRATB(I, IDPA) = GRAT(I)
  200   CONTINUE
C
        DO 300 I = 1, 2
          BCURB(I, IDPA) = BCUR(I)
  300   CONTINUE
C
  400 CONTINUE
C
      IER = 0
C
  500 CALL OMBOUT (XIPB, IBUNB, BCURB, DEFCOR, IER)
C
      RETURN
      END
#endif
