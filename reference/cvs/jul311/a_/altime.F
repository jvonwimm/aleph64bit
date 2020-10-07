      SUBROUTINE ALTIME (TIME)
C----------------------------------------------------------------------
C!  - Pseudo TIMEX for falcon
C!
C!   Modified :- E. Lancon             31-AUG-1993
C!          To use TIMEL (TIMEX does not work on the AXP)
C!
C?
C!======================================================================
#ifndef DOC
C
      CALL TIMEL (TIME)
      TIME = - TIME
C
  999 RETURN
      END
#endif
