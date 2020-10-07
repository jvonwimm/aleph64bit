      SUBROUTINE QVSETM (ITK,AMASS)
CKEY MASS KINEMATICS /USER
C----------------------------------------------------------------------
C! AMASS --> mass(ITK).
C                                                    H.Albrecht  2.1.89
C  Modified to get the correct error matrix          P.Heusse   28.4.95
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
      JR(J) = KOQVEC + ITK * KCQVEC + J
C----------------------------------------------------------------------
C
      RW(JR(JQVEQM)) = AMASS
      FAC_COR = RW (JR(JQVEQE))
      RW(JR(JQVEQE)) = SQRT (RW(JR(JQVEQM))**2 + RW(JR(JQVEQP))**2)
      FAC_COR = FAC_COR/RW (JR(JQVEQE))
      RW(JR(JQVEEM)+6 ) = RW(JR(JQVEEM)+6 )*FAC_COR
      RW(JR(JQVEEM)+7 ) = RW(JR(JQVEEM)+7 )*FAC_COR
      RW(JR(JQVEEM)+8 ) = RW(JR(JQVEEM)+8 )*FAC_COR
      RW(JR(JQVEEM)+9 ) = RW(JR(JQVEEM)+9 )*FAC_COR*FAC_COR
C
      END
#endif
