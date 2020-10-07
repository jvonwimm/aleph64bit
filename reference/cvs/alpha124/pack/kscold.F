      FUNCTION KSCOLD (ICLASS)
CKEY CLASS /INTERNAL
C----------------------------------------------------------------------
C! Get sequential class number for ICLASS. RETURN 0 if not existing.
C!                                                  H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      IF (ICLASS .EQ. KRECO)  THEN
        KSCOLD = 1
      ELSE IF (ICLASS .EQ. KMONTE)  THEN
        KSCOLD = 2
      ELSE IF (ICLASS .EQ. KCLACO(3))  THEN
        KSCOLD = 3
      ELSE IF (ICLASS .EQ. KCLACO(4))  THEN
        KSCOLD = 4
      ELSE IF (ICLASS .EQ. KCLACO(5))  THEN
        KSCOLD = 5
      ELSE IF (ICLASS .EQ. KCLACO(6))  THEN
        KSCOLD = 6
      ELSE IF (ICLASS .EQ. KCLACO(7))  THEN
        KSCOLD = 7
      ELSE IF (ICLASS .EQ. KCLACO(8))  THEN
        KSCOLD = 8
      ELSE
        KSCOLD = 0
      ENDIF
C
   90 END
#endif
