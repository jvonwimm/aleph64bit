      CHARACTER*4 FUNCTION NLISTB(JW,N,LIST)
C! copy of NLIST to avoid confusion with MULTINET TCP/IP
C     RETURN N-TH NAME OF LIST
C
      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
      CHARACTER*(*) LIST
      CHARACTER*4 NAME,CHAINT
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'LISTI')
      NLISTB='    '
      IF(N.LE.0) GOTO 100
      NN=0
#include "listnn.h"
      IF(NAMI.EQ.0) GOTO 100
      NN=NN+1
      IF(N.NE.NN) GOTO 3
      NLISTB=CHAINT(IW(IDNAM+NAMI-NSYST))
  100 RETURN
      END
