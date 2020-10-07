      FUNCTION KSCNEW (ICLASS)
CKEY CLASS /INTERNAL
C----------------------------------------------------------------------
C! Get sequential class number for ICLASS. New class if not existing.
C!                                                  H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      IF (ICLASS .EQ. KRECO)  THEN
        KSCNEW = 1
      ELSE IF (ICLASS .EQ. KMONTE)  THEN
        KSCNEW = 2
      ELSE IF (ICLASS .EQ. KCLACO(3))  THEN
        KSCNEW = 3
      ELSE IF (ICLASS .EQ. KCLACO(4))  THEN
        KSCNEW = 4
      ELSE IF (ICLASS .EQ. KCLACO(5))  THEN
        KSCNEW = 5
      ELSE IF (ICLASS .EQ. KCLACO(6))  THEN
        KSCNEW = 6
      ELSE IF (ICLASS .EQ. KCLACO(7))  THEN
        KSCNEW = 7
      ELSE IF (ICLASS .EQ. KCLACO(8))  THEN
        KSCNEW = 8
      ELSE
C
        DO 10 KSCNEW=3,KCQFPA
          IF (KCLACO(KSCNEW) .EQ. KUNDEF)  THEN
            KCLACO(KSCNEW) = ICLASS
            KMQFPA = MAX (KMQFPA, KSCNEW)
            GO TO 90
          ENDIF
   10   CONTINUE
C
        CALL QMTERM
     &    ('_KSCNEW_ Maximum number of classes exceeded')
      ENDIF
C
   90 END
#endif
