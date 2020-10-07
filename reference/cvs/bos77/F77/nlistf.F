      CHARACTER*4 FUNCTION NLISTF(JW,N,LIST)
C
C     RETURN N-TH NAME OF LIST
C     Fast version - LIST must be character * 1 !  H.Albrecht
C
      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
      CHARACTER*(*) LIST
      CHARACTER*4 NAME,CHAINT
C     ...
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'LISTI')
      IF(N.LE.0) GOTO 100
C
      JLS=INDEX('CERST',LIST)
      IF(JLS.EQ.0) GO TO 100
      JLS=JLS+ILT
      IF(N.GT.JW(JW(JLS))) GO TO 100
      NAMI=JW(JW(JLS)+N)
      NLISTF=CHAINT(IW(IDNAM+NAMI-NSYST))
      RETURN
  100 NLISTF = '    '
      END
