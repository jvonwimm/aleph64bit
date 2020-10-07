      SUBROUTINE ABOLDR ( IRUN )
C----------------------------------------------------------------------
C!  - routine called at end of run by ABRSEL before reading next run rec
C!
C!   Author   :- E. Lancon             28-AUG-1992
C!   Modified :- E. Lancon              7-JUN-1993
C!       Common Deck VERSION MUST be there, this routine should be called
C!       and not the alephlib routine. With VERSION it will be in the
C!       correction file and loaded before the alephlib routine.
C!   Modified :- M. Cattaneo           31-MAY-1996
C!       VERSION removed, since we no longer have correction files with cvs.
C!
C!   Inputs:
C!        - IRUN           I/  Run Number
C!
C?
C!======================================================================
#ifndef DOC
C
      CALL RCLRUN
  999 RETURN
      END
#endif
