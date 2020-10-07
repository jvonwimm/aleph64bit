      SUBROUTINE QVSCAL (ITK,FACT)
CKEY VECTOR KINEMATICS/USER
C----------------------------------------------------------------------
C! momentum(ITK) --> momentum(ITK) * FACT
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      JR(J) = KOQVEC + ITK * KCQVEC + J
C----------------------------------------------------------------------
C
      RW(JR(JQVEQX)) = RW(JR(JQVEQX)) * FACT
      RW(JR(JQVEQY)) = RW(JR(JQVEQY)) * FACT
      RW(JR(JQVEQZ)) = RW(JR(JQVEQZ)) * FACT
      RW(JR(JQVEQP)) = RW(JR(JQVEQP)) * ABS (FACT)
      RW(JR(JQVEQE)) = SQRT (RW(JR(JQVEQM))**2 + RW(JR(JQVEQP))**2)
C
      END
#endif
