      SUBROUTINE QCPMOD (CPAR,CANT,AMA,ACH,ALI,AWI)
CKEY PART /INTERNAL
C----------------------------------------------------------------------
C! modify particle according to PMOD data card
C called from QMPART
C                                                   H.Albrecht 15.01.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER * (*) CPAR,CANT
C----------------------------------------------------------------------
C
      CALL QAPNAM (CPAR,CANT,1,IPAR,IANT)
C
      MODI = 0
      IF (AMA .NE. -9999.)  THEN
        RW(KOQPAR+IPAR*KCQPAR+JQPAMA) = AMA
        RW(KOQPAR+IANT*KCQPAR+JQPAMA) = AMA
        MODI = 1
      ENDIF
      IF (ACH .NE. -9999.)  THEN
        RW(KOQPAR+IPAR*KCQPAR+JQPACH) = ACH
        RW(KOQPAR+IANT*KCQPAR+JQPACH) = - ACH
        MODI = 1
      ENDIF
      IF (ALI .NE. -9999.)  THEN
        RW(KOQPAR+IPAR*KCQPAR+JQPALT) = ALI
        RW(KOQPAR+IANT*KCQPAR+JQPALT) = ALI
        MODI = 1
      ENDIF
      IF (AWI .NE. -9999.)  THEN
        RW(KOQPAR+IPAR*KCQPAR+JQPAWI) = AWI
        RW(KOQPAR+IANT*KCQPAR+JQPAWI) = AWI
        MODI = 1
      ENDIF
C
      IF (MODI .NE. 0)  THEN
        IW(KOQPBT+IPAR) = IOR (IW(KOQPBT+IPAR), KBIT(2))
        IW(KOQPBT+IANT) = IOR (IW(KOQPBT+IANT), KBIT(2))
      ENDIF
C
      END
#endif
