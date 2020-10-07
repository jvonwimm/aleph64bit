      SUBROUTINE QLTRK (ITK)
CKEY LOCK /USER
C----------------------------------------------------------------------
C! set lock flag for track ITK.
C
C called from user routines
C                                                   H.Albrecht 27.11.88
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C----------------------------------------------------------------------
C
      I = ITK
 10   IW(KOQVEC+I*KCQVEC+JQVELK) = 1
      I = IW(KOQVEC+I*KCQVEC+JQVESP)
      IF (I .NE. ITK)  GO TO 10
      END
#endif
