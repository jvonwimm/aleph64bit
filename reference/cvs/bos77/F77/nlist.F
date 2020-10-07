      CHARACTER*4 FUNCTION NLIST(JW,N,LIST)
C
C     RETURN N-TH NAME OF LIST
C
      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
      CHARACTER*(*) LIST
      CHARACTER*4 NAME,CHAINT
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'LISTI')
      NLIST='    '
      IF(N.LE.0) GOTO 100
      NN=0
#include "listnn.h"
      IF(NAMI.EQ.0) GOTO 100
      NN=NN+1
      IF(N.NE.NN) GOTO 3
      NLIST=CHAINT(IW(IDNAM+NAMI-NSYST))
  100 RETURN
      END
