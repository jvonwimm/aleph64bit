      SUBROUTINE TSZCOR(ITE,TIME,RPH,N,ZB)
C
C-----------------------------------------------------------------------
C! Calculate cluster z coordinate
C!
C!    Author:    R. Johnson  03-08-87
C!    Modified:  R. Johnson  08-04-88
C!
C!    Input:
C!       - ISLOT     /I     Sector slot number
C!       - ITE       /I     Algorithm used to calculate TIME
C!       - TIME      /R     List of time estimates from adjacent pads
C!       - RPH       /R     List of charge estimates from adjacent pads
C!       - N         /I     Number of pads
C!    Output:
C!       - ZB        /R     Z position in units of time buckets
C!
C!    Called by TCOOR
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "tzcorr.h"
C
      DIMENSION TIME(N),RPH(N)
C
C++   Calculate a charge weighted average of the N time values
C
      CHSUM=0.
      TSUM=0.
      DO 100 I=1,N
        TSUM=TSUM + RPH(I)*TIME(I)
        CHSUM=CHSUM+RPH(I)
  100 CONTINUE
      TAVG= TSUM/CHSUM
C
C++   Correct z for offset due to time algorithm used
C
      ZB = TAVG + TZOFFS(ITE)
C
      RETURN
      END
#endif
