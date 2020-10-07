      SUBROUTINE QVSETS (ITK,S)
CKEY ERROR COPY /USER
C----------------------------------------------------------------------
C! error matrix S --> ITK
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      DIMENSION S(4,4)
      J1(J) = KOQVEC + ITK * KCQVEC + J
C----------------------------------------------------------------------
C
      RW(J1(JQVEEM  )) = S(1,1)
      RW(J1(JQVEEM+1)) = S(1,2)
      RW(J1(JQVEEM+2)) = S(2,2)
      RW(J1(JQVEEM+3)) = S(1,3)
      RW(J1(JQVEEM+4)) = S(2,3)
      RW(J1(JQVEEM+5)) = S(3,3)
      RW(J1(JQVEEM+6)) = S(1,4)
      RW(J1(JQVEEM+7)) = S(2,4)
      RW(J1(JQVEEM+8)) = S(3,4)
      RW(J1(JQVEEM+9)) = S(4,4)
C
      END
#endif
