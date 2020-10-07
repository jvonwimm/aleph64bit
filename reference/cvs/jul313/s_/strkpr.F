      SUBROUTINE STRKPR(LUN)
C----------------------------------------------------------------------
C! Print the STRK bank
C!
C!    Author:     H. Meinhard       10-Feb-1988
C!    Modified:   H. Meinhard       09-Oct-1989  (3)
C!
C!    Input:      - LUN       /I    logical unit for output
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "sanamc.h"
#include "stcpjj.h"
#include "strkjj.h"
      CHARACTER*1 CAB(2), CLH(2)
      DATA CAB/'A','B'/, CLH/'L','H'/
#include "bmacro.h"
C----------------------------------------------------------------------
C link to banks
      KSTRK = IW(NASTRK)
      KSTCP = IW(NASTCP)
      IF (KSTRK .EQ. 0)                                     GOTO 901
C
C if pointers to coordinates available, choose full output format
      IF (KSTCP .NE. 0) THEN
C
C if banks exist, but no entries, print corresponding message
        IF (LROWS(KSTRK) .EQ. 0) THEN
          WRITE (LUN,500)
        ELSE
          WRITE (LUN,510)
          DO 300 ITR = 1, LROWS(KSTRK)
            THETA = RTABL(KSTRK,ITR,JSTRTH)
            PHI   = RTABL(KSTRK,ITR,JSTRPH)
            IF (THETA .LT. PIBY2) THEN
              ISIDE = 1
            ELSE
              ISIDE = 2
              THETA = PI - THETA
              PHI   = MOD(PHI+PI,TWOPI)
            ENDIF
            NPAR  = ITABL(KSTRK,ITR,JSTRNP)
            NPAR1 = MIN(NPAR,9)
            CHISQ = MAX(RTABL(KSTRK,ITR,JSTRC2),1.E-12)
            IF (NPAR .GT. 2) THEN
              CHPRB = PROB(CHISQ,NPAR-2)
            ELSE
              CHPRB = 0.
            ENDIF
            WRITE (LUN,520) ITR,CAB(ISIDE),ITABL(KSTRK,ITR,JSTRQF),
     +        CHPRB,
     +      THETA,PHI,RTABL(KSTRK,ITR,JSTRTT),RTABL(KSTRK,ITR,JSTRTP),
     +      RTABL(KSTRK,ITR,JSTRPP),(ITABL(KSTCP,ITR,I)/2,
     +      CLH(MOD(ITABL(KSTCP,ITR,I),2)+1),I=1,NPAR1)
  300     CONTINUE
        ENDIF
C
      ELSE
C
C no pointers to coordinates. Print only STRK bank
C
C if banks exist, but no entries, print corresponding message
        IF (LROWS(KSTRK) .EQ. 0) THEN
          WRITE (LUN,500)
        ELSE
          WRITE (LUN,610)
          DO 310 ITR = 1, LROWS(KSTRK)
            THETA = RTABL(KSTRK,ITR,JSTRTH)
            PHI   = RTABL(KSTRK,ITR,JSTRPH)
            IF (THETA .LT. PIBY2) THEN
              ISIDE = 1
            ELSE
              ISIDE = 2
              THETA = PI - THETA
              PHI   = MOD(PHI+PI,TWOPI)
            ENDIF
            NPAR  = ITABL(KSTRK,ITR,JSTRNP)
            NPAR1 = MIN(NPAR,9)
            CHISQ = MAX(RTABL(KSTRK,ITR,JSTRC2),1.E-12)
            IF (NPAR .GT. 2) THEN
              CHPRB = PROB(CHISQ,NPAR-2)
            ELSE
              CHPRB = 0.
            ENDIF
            WRITE (LUN,620) ITR,CAB(ISIDE),ITABL(KSTRK,ITR,JSTRQF),
     +        CHPRB,
     +      THETA,PHI,RTABL(KSTRK,ITR,JSTRTT),RTABL(KSTRK,ITR,JSTRTP),
     +      RTABL(KSTRK,ITR,JSTRPP),NPAR
  310     CONTINUE
        ENDIF
C
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  901 IF (KSTRK .EQ. 0) WRITE (LUN,530) 'STRK'
      IF (KSTCP .EQ. 0) WRITE (LUN,530) 'STCP'
      GOTO 999
  999 CONTINUE
      RETURN
C
C Formats
  500 FORMAT (9X,'No tracks found.')
  510 FORMAT (
     +  '  Nr  Side  QF  chi2-p    th/pi-th   ph/pi+ph    cov tt',
     +  '       cov tp       cov pp     coordinates used (L = lower, ',
     +  'H = higher theta)')
  520 FORMAT (I4,4X,A1,I5,F8.4,E13.4,F9.4,3E13.4,2X,9(1X,I3,A1))
  530 FORMAT (' --> STRKPR: Bank ',A4,' not found')
  610 FORMAT (
     +  '  Nr  Side  QF  chi2-p    th/pi-th   ph/pi+ph    cov tt',
     +  '       cov tp       cov pp   npar')
  620 FORMAT (I4,4X,A1,I5,F8.4,E13.4,F9.4,3E13.4,2X,I1)
      END
#endif
