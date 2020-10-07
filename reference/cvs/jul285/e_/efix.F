      SUBROUTINE EFIX
C----------------------------------------------------------------------
C!  -
C!    temporary fix until ALEPHLIB is corrected
C     needed for machines on which memory is not preset to 0
C?
C!======================================================================
#ifndef DOC
      COMMON/EVOLCC/DUMM(192),IVOL(43),D2(43)
      COMMON/ETYVCC/DUM(8),ITY(6),D3(6)
      CALL VZERO(IVOL,43)
      CALL VZERO(ITY,6)
      RETURN
      END
#endif
