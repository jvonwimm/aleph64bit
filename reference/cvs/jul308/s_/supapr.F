      SUBROUTINE SUPAPR(LUN)
C----------------------------------------------------------------------
C! Print the SUPA bank
C!
C!    Author:     H. Meinhard       29-Mar-1988
C!    Modified:   H. Meinhard       04-Jul-1989  (1)
C!
C!    Input:      - LUN       /I    logical unit for output
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "sanamc.h"
#include "sucpjj.h"
#include "supajj.h"
      CHARACTER*1 CAB(2)
      DATA CAB/'A','B'/
#include "bmacro.h"
C----------------------------------------------------------------------
C link to banks
      KSUPA = IW(NASUPA)
      KSUCP = IW(NASUCP)
      IF (KSUPA.EQ.0 .OR. KSUCP.EQ.0)                       GOTO 901
C
C if banks exist, but no entries, print corresponding message
      IF (LROWS(KSUPA) .EQ. 0) THEN
        WRITE (LUN,500)
      ELSE
        WRITE (LUN,510)
        DO 300 IUP = 1, LROWS(KSUPA)
          THETA = RTABL(KSUPA,IUP,JSUPTH)
          PHI   = RTABL(KSUPA,IUP,JSUPPH)
          IF (THETA .LT. PIBY2) THEN
            ISIDE = 1
          ELSE
            ISIDE = 2
            THETA = PI - THETA
            PHI   = MOD(PHI+PI,TWOPI)
          ENDIF
          NPAR  = ITABL(KSUPA,IUP,JSUPNP)
          NPAR1 = MIN(NPAR,9)
          CHISQ = MAX(RTABL(KSUPA,IUP,JSUPC2),1.E-12)
          IF (NPAR .GT. 2) THEN
            CHPRB = PROB(CHISQ,NPAR-2)
          ELSE
            CHPRB = 0.
          ENDIF
          WRITE (LUN,520) IUP,CAB(ISIDE),CHPRB,THETA,PHI,
     +      RTABL(KSUPA,IUP,JSUPTT),RTABL(KSUPA,IUP,JSUPTP),
     +      RTABL(KSUPA,IUP,JSUPPP),(ITABL(KSUCP,IUP,I),
     +      I=1,NPAR1)
  300   CONTINUE
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  901 IF (KSUPA .EQ. 0) WRITE (LUN,530) 'SUPA'
      IF (KSUCP .EQ. 0) WRITE (LUN,530) 'SUCP'
      GOTO 999
  999 CONTINUE
      RETURN
C
C Formats
  500 FORMAT (9X,'No patches found.')
  510 FORMAT (
     +  '  Nr   Side   Chisq-Prob   th/pi-th   ph/pi+ph    cov tt',
     +  '       cov tp       cov pp              coordinates used ')
  520 FORMAT (I4,5X,A1,F11.4,E15.4,F9.4,3E13.4,2X,9(1X,I3))
  530 FORMAT (' --> SUPAPR: Bank ',A4,' not found')
      END
#endif
