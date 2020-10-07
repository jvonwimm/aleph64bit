      SUBROUTINE QTCLAS (ICLASS,ITKRST)
CKEY KINEMATICS /USER
C----------------------------------------------------------------------
C! Lorentz transformation of all objects in ICLASS into frame ITKRST.
C                                                    H.Albrecht 22.2.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
#include "qctbuf.h"
C
      ICLRST = KSCNEW (ITKRST)
      CALL QJSET0 (ICLASS)
C
      DO 10 I=1,KTBI
   10 JDUMMY = KTLORX (KTBIT(I),ICLRST,IDUMMY,ITKRST)
C
      END
#endif
