      FUNCTION KPDIRA (CPAR,ICLASS,IANTI)
CKEY PART /USER
C----------------------------------------------------------------------
C! Get first particle CPAR or its antip. in class ICLASS.
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER *(*) CPAR
C
      IDUM=KPART(CPAR)
      IF (IANTI .NE. 0)  THEN
        KPDIRA = IW(KOQPLI+IW(KOQPAR+IDUM*KCQPAR+JQPAAN))
      ELSE
        KPDIRA = IW(KOQPLI+IDUM)
      ENDIF
      IF (KPDIRA .EQ. 0)  GO TO 90
C
      IF (ICLASS .EQ. KRECO)  THEN
        KPDIRA = IW(KOQFPA+KPDIRA*KCQFPA+1)
      ELSE IF (ICLASS .EQ. KMONTE)  THEN
        KPDIRA = IW(KOQFPA+KPDIRA*KCQFPA+2)
      ELSE IF (ICLASS .EQ. KCLACO(3))  THEN
        KPDIRA = IW(KOQFPA+KPDIRA*KCQFPA+3)
      ELSE IF (ICLASS .EQ. KCLACO(4))  THEN
        KPDIRA = IW(KOQFPA+KPDIRA*KCQFPA+4)
      ELSE IF (ICLASS .EQ. KCLACO(5))  THEN
        KPDIRA = IW(KOQFPA+KPDIRA*KCQFPA+5)
      ELSE IF (ICLASS .EQ. KCLACO(6))  THEN
        KPDIRA = IW(KOQFPA+KPDIRA*KCQFPA+6)
      ELSE IF (ICLASS .EQ. KCLACO(7))  THEN
        KPDIRA = IW(KOQFPA+KPDIRA*KCQFPA+7)
      ELSE IF (ICLASS .EQ. KCLACO(8))  THEN
        KPDIRA = IW(KOQFPA+KPDIRA*KCQFPA+8)
      ELSE
        KPDIRA = 0
      ENDIF
C
   90 END
#endif
