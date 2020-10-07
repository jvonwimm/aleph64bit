*DK silong
      FUNCTION SILONG(S)
C--------------------------------------------------------------------
C! Generates longitudinal em-shower profile
C  Author: J.Rander for Sical , inspired from Ecal by Jean Badier
C
C       Input : Depth in radiation lengths
C       Output : Profile
C       Used by : SISHOW
C====================================================================
#ifndef DOC
#include "siparm.h"
C
      IF(S .LE.0.0) GO TO 1
      X = SIALPH(2) * S
      IF(ABS(X) .GT. 100.) GO TO 1
      A = -X + (SIALPH(1)-1.) * ALOG(S)
      SILONG = EXP(A)
      GO TO 98
   1  SILONG = 0.
   98 CONTINUE
      RETURN
      END
#endif
