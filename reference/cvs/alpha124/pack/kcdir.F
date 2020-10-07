      FUNCTION KCDIR (IPC,ICLASS)
CKEY PART /USER
C----------------------------------------------------------------------
C! Get first particle IPC in class ICLASS.
C!                                                  H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      KCDIR = IW(KOQPLI+IPC)
      IF (KCDIR .EQ. 0)  GO TO 90
C
      IF (ICLASS .EQ. KRECO)  THEN
        KCDIR = IW(KOQFPA+KCDIR*KCQFPA+1)
      ELSE IF (ICLASS .EQ. KMONTE)  THEN
        KCDIR = IW(KOQFPA+KCDIR*KCQFPA+2)
      ELSE IF (ICLASS .EQ. KCLACO(3))  THEN
        KCDIR = IW(KOQFPA+KCDIR*KCQFPA+3)
      ELSE IF (ICLASS .EQ. KCLACO(4))  THEN
        KCDIR = IW(KOQFPA+KCDIR*KCQFPA+4)
      ELSE IF (ICLASS .EQ. KCLACO(5))  THEN
        KCDIR = IW(KOQFPA+KCDIR*KCQFPA+5)
      ELSE IF (ICLASS .EQ. KCLACO(6))  THEN
        KCDIR = IW(KOQFPA+KCDIR*KCQFPA+6)
      ELSE IF (ICLASS .EQ. KCLACO(7))  THEN
        KCDIR = IW(KOQFPA+KCDIR*KCQFPA+7)
      ELSE IF (ICLASS .EQ. KCLACO(8))  THEN
        KCDIR = IW(KOQFPA+KCDIR*KCQFPA+8)
      ELSE
        KCDIR = 0
      ENDIF
C
   90 END
#endif
