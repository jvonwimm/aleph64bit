      SUBROUTINE NGPUT(RM,IE,RO)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : auxiliary to NREPAR
C!
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!
C!   Inputs:
C!        - RM   / F    mantisse of a floating point number
C!                      has to be in the range -1.0.lt.RM.lt.1.0
C!          IE   / I    exponent of a floating point number
C!                      has to be in the range -37.le.IE.le.37
C!
C!   Outputs:
C!        - RO   / F    RO=RM*(10.0**IE)
C!
C!   Libraries required: NONE
C!
C!======================================================================
      INTEGER IE
      REAL    RM,RO
      RO=0.0
      IF(ABS(IE).GT.37)GOTO 999
      IF(ABS(RM).GE.1.0)GOTO 999
      RO=RM*(10.0**IE)
  999 RETURN
      END
