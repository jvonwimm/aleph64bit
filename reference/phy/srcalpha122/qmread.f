      SUBROUTINE QMREAD
CKEY EVENT /INTERNAL
C----------------------------------------------------------------------
C!  - Calling routine for QMRDSB, QMTERM
C!
C!   Author   :- E. Blucher     27-JUL-1990
C!   Description
C!   ===========
C! Calls QMRDSB and QMTERM.  Added for compatability with
C! interactive ALPHA.
C!======================================================================
      CALL QMRDSB(IEND)
      IF(IEND.EQ.1)CALL QMTERM('_QMREAD_ End-of-job ')
  999 RETURN
      END
