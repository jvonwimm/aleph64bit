      FUNCTION KCDIRA (IPC,ICLASS,IANTI)
CKEY PART /USER
C----------------------------------------------------------------------
C! Get first particle IPC or its antip. in class ICLASS.
C!                                                  H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      IF (IANTI .NE. 0)  THEN
        KCDIRA = IW(KOQPLI+IW(KOQPAR+IPC*KCQPAR+JQPAAN))
      ELSE
        KCDIRA = IW(KOQPLI+IPC)
      ENDIF
      IF (KCDIRA .EQ. 0)  GO TO 90
C
      IF (ICLASS .EQ. KRECO)  THEN
        KCDIRA = IW(KOQFPA+KCDIRA*KCQFPA+1)
      ELSE IF (ICLASS .EQ. KMONTE)  THEN
        KCDIRA = IW(KOQFPA+KCDIRA*KCQFPA+2)
      ELSE IF (ICLASS .EQ. KCLACO(3))  THEN
        KCDIRA = IW(KOQFPA+KCDIRA*KCQFPA+3)
      ELSE IF (ICLASS .EQ. KCLACO(4))  THEN
        KCDIRA = IW(KOQFPA+KCDIRA*KCQFPA+4)
      ELSE IF (ICLASS .EQ. KCLACO(5))  THEN
        KCDIRA = IW(KOQFPA+KCDIRA*KCQFPA+5)
      ELSE IF (ICLASS .EQ. KCLACO(6))  THEN
        KCDIRA = IW(KOQFPA+KCDIRA*KCQFPA+6)
      ELSE IF (ICLASS .EQ. KCLACO(7))  THEN
        KCDIRA = IW(KOQFPA+KCDIRA*KCQFPA+7)
      ELSE IF (ICLASS .EQ. KCLACO(8))  THEN
        KCDIRA = IW(KOQFPA+KCDIRA*KCQFPA+8)
      ELSE
        KCDIRA = 0
      ENDIF
C
   90 END
#endif
