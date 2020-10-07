      SUBROUTINE SKANPR(LUN)
C----------------------------------------------------------------------
C! Print the SKAN bank
C!
C!    Author:     H. Meinhard       28-Jan-1988
C!    Modified:   H. Meinhard       04-Jul-1989  (1)
C!
C!    Input:      - LUN       /I    Logical unit for output
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "sanamc.h"
#include "skanjj.h"
#include "skcpjj.h"
      CHARACTER*1 CAB(2),CLH(2)
      EXTERNAL PROB
      DATA CAB/'A','B'/, CLH/'L','H'/
#include "bmacro.h"
C----------------------------------------------------------------------
C link to banks
      KSKAN = IW(NASKAN)
      KSKCP = IW(NASKCP)
      IF (KSKAN.EQ.0 .OR. KSKCP.EQ.0)                       GOTO 901
C
C if banks exist, but no entries, print corresponding messages
      IF (LROWS(KSKAN) .EQ. 0) THEN
        WRITE (LUN,500)
      ELSE
        WRITE (LUN,510)
        DO 300 IKAN = 1, LROWS(KSKAN)
          ISIDE = ITABL(KSKAN,IKAN,JSKASI)
          NPAR  = ITABL(KSKAN,IKAN,JSKANP)
          CHPRB = PROB(RTABL(KSKAN,IKAN,JSKAC2),NPAR-2)
          AX    = RTABL(KSKAN,IKAN,JSKAAX)
          AY    = RTABL(KSKAN,IKAN,JSKAAY)
          XX    = RTABL(KSKAN,IKAN,JSKAXX)
          XY    = RTABL(KSKAN,IKAN,JSKAXY)
          YY    = RTABL(KSKAN,IKAN,JSKAYY)
          AX2   = AX*AX
          AY2   = AY*AY
          AXAY  = AX*AY
          THETA = ATAN(SQRT(AX2+AY2))
          IF (ISIDE .EQ. 1) THEN
            PHI = MOD(ATAN2(AY,AX)+TWOPI,TWOPI)
          ELSE
            PHI = MOD(ATAN2(AY,AX)+PI,TWOPI)
          ENDIF
          COVTT = (XX*AX2 + 2.*XY*AXAY + YY*AY2) / THETA**2
          COVTP = (-XX*AXAY + XY*(AX2-AY2) + YY*AXAY) / THETA**3
          COVPP = (XX*AY2 - 2.*XY*AXAY + YY*AX2) / THETA**4
          WRITE (LUN,520) IKAN,CAB(ISIDE),ITABL(KSKCP,IKAN,JSKCKM),
     +      CHPRB,THETA,PHI,COVTT,COVTP,COVPP,NPAR,
     +      (ITABL(KSKCP,IKAN,JSKCKC+I-1)/2,
     +      CLH(MOD(ITABL(KSKCP,IKAN,JSKCKC+I-1),2)+1),I=1,NPAR)
  300   CONTINUE
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  901 IF (KSKAN .EQ. 0) WRITE (LUN,530) 'SKAN'
      IF (KSKCP .EQ. 0) WRITE (LUN,530) 'SKCP'
      GOTO 999
  999 CONTINUE
      RETURN
C
C Formats
  500 FORMAT (9X,'No track candidates found.')
  510 FORMAT ('  Nr  Side  match  chi2-p  th/pi-th  ph/pi+ph   cov tt',
     +        '      cov tp      cov pp    #  coordinates used (L = ',
     +        'lower, H = higher theta)')
  520 FORMAT (I4,4X,A1,I6,F10.4,E11.4,F8.4,3E12.4,I3,2X,9(1X,I3,A1))
  530 FORMAT (' --> SKANPR: BANK ',A4,' NOT FOUND')
      END
#endif
