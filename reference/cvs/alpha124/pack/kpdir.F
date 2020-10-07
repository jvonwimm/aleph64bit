      FUNCTION KPDIR (CPAR,ICLASS)
CKEY PART CLASS /USER
C----------------------------------------------------------------------
C! Get first particle CPAR in class ICLASS.
C                                                   H.Albrecht 27.11.88
C                                    Modified:      E.Blucher   26.2.90
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      CHARACTER *(*) CPAR
C
      IDUM=KPART(CPAR)
      KPDIR = IW(KOQPLI+IDUM)
      IF (KPDIR .EQ. 0)  GO TO 90
C
      IF (ICLASS .EQ. KRECO)  THEN
        KPDIR = IW(KOQFPA+KPDIR*KCQFPA+1)
      ELSE IF (ICLASS .EQ. KMONTE)  THEN
        KPDIR = IW(KOQFPA+KPDIR*KCQFPA+2)
      ELSE IF (ICLASS .EQ. KCLACO(3))  THEN
        KPDIR = IW(KOQFPA+KPDIR*KCQFPA+3)
      ELSE IF (ICLASS .EQ. KCLACO(4))  THEN
        KPDIR = IW(KOQFPA+KPDIR*KCQFPA+4)
      ELSE IF (ICLASS .EQ. KCLACO(5))  THEN
        KPDIR = IW(KOQFPA+KPDIR*KCQFPA+5)
      ELSE IF (ICLASS .EQ. KCLACO(6))  THEN
        KPDIR = IW(KOQFPA+KPDIR*KCQFPA+6)
      ELSE IF (ICLASS .EQ. KCLACO(7))  THEN
        KPDIR = IW(KOQFPA+KPDIR*KCQFPA+7)
      ELSE IF (ICLASS .EQ. KCLACO(8))  THEN
        KPDIR = IW(KOQFPA+KPDIR*KCQFPA+8)
      ELSE
        KPDIR = 0
      ENDIF
C
   90 END
#endif
