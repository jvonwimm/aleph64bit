      SUBROUTINE SFTRPR(LUN)
C----------------------------------------------------------------------
C! Print the SFTR bank
C!
C!    Author:     H. Meinhard       05-Mar-1990
C!
C!    Input:      - LUN       /I    logical unit for printout
C!                - banks 'SFTR','STCP'
C----------------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "sanamc.h"
#include "sftrjj.h"
#include "stcpjj.h"
      CHARACTER*1 CAB(2),CLH(2)
      DATA CAB/'A','B'/, CLH/'L','H'/
#include "bmacro.h"
C----------------------------------------------------------------------
C link to banks
      JSFTR = IW(NASFTR)
      JSTCP = IW(NASTCP)
      IF (JSFTR .EQ. 0)                                     GOTO 901
C if banks exist, but no entries, print corresponding message
      IF (LROWS(JSFTR) .EQ. 0) THEN
        WRITE (LUN,500)
      ELSE
C if pointers to coordinates available, choose full output format
        IF (JSTCP .NE. 0) THEN
          WRITE (LUN,510)
          DO 310 ITR = 1, LROWS(JSFTR)
            THETA = RTABL(JSFTR,ITR,JSFTTH)
            PHI = RTABL(JSFTR,ITR,JSFTPH)
            IF (THETA .LT. PIBY2) THEN
              ISIDE = 1
            ELSE
              ISIDE = 2
              THETA = PI - THETA
              PHI = MOD(PHI+PI,TWOPI)
            ENDIF
            IQF = ITABL(JSFTR,ITR,JSFTQF)
            IF (IQF .EQ. 0) THEN
              NDF = 0
              CHPRB = 0.
            ELSE
              NDF = ITABL(JSFTR,ITR,JSFTNF)
              CHPRB = PROB(RTABL(JSFTR,ITR,JSFTC2),NDF)
            ENDIF
C count wire parallels
            NPAR = 0
            DO 300 IPAR = 1, LSTCPA
  300       IF (ITABL(JSTCP,ITR,IPAR) .NE. 0) NPAR = NPAR + 1
            WRITE (LUN,520) ITR,CAB(ISIDE),IQF,NDF,CHPRB,THETA,PHI,
     +        RTABL(JSFTR,ITR,JSFTX0),RTABL(JSFTR,ITR,JSFTY0),
     +        (ITABL(JSTCP,ITR,I)/2,CLH(MOD(ITABL(JSTCP,ITR,I),2)+1),
     +        I=1, NPAR)
  310     CONTINUE
        ELSE
C no printers to coordinates. Print only SFTR bank
          WRITE (LUN,610)
          DO 320 ITR = 1, LROWS(JSFTR)
            THETA = RTABL(JSFTR,ITR,JSFTTH)
            PHI = RTABL(JSFTR,ITR,JSFTPH)
            IF (THETA .LT. PIBY2) THEN
              ISIDE = 1
            ELSE
              ISIDE = 2
              THETA = PI - THETA
              PHI = MOD(PHI+PI,TWOPI)
            ENDIF
            IQF = ITABL(JSFTR,ITR,JSFTQF)
            IF (IQF .EQ. 0) THEN
              NDF = 0
              CHPRB = 0.
            ELSE
              NDF = ITABL(JSFTR,ITR,JSFTNF)
              CHPRB = PROB(RTABL(JSFTR,ITR,JSFTC2),NDF)
            ENDIF
            WRITE (LUN,520) ITR,CAB(ISIDE),IQF,NDF,CHPRB,THETA,PHI,
     +        RTABL(JSFTR,ITR,JSFTX0),RTABL(JSFTR,ITR,JSFTY0)
  320     CONTINUE
        ENDIF
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  901 WRITE (LUN,530)
      GOTO 999
  500 FORMAT (9X,'No tracks found.')
  510 FORMAT (
     +  '  Nr  Side  QF  NDF  chi2-p    th/pi-th   ph/pi+ph     x0   ',
     +  '    y0    coordinates used (L = lower, ',
     +  'H = higher theta)')
  520 FORMAT (I4,4X,A1,I5,I4,F9.4,E13.4,F9.4,1X,2F9.3,2X,9(1X,I3,A1))
  530 FORMAT (' --> SFTRPR: Bank SFTR not found')
  610 FORMAT (
     +  '  Nr  Side  QF  NDF  chi2-p    th/pi-th   ph/pi+ph     x0   ',
     +  '    y0')
  620 FORMAT (I4,4X,A1,I5,I4,F9.4,E13.4,F9.4,1X,2F9.3)
  999 CONTINUE
      END
#endif
