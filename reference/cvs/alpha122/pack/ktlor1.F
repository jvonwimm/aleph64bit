      FUNCTION KTLOR1 (ITK,ITKRST)
CKEY KINEMATICS /USER
C----------------------------------------------------------------------
C! Boost ITK into the rest frame of ITKRST.
C                                                    H.Albrecht  2.1.89
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C
      ICL = KSCNEW (ITKRST)
C
      KTLOR1 = KTLORX (ITK,ICL,IDUMMY,ITKRST)
C
      END
#endif
