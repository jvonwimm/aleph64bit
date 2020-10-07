      SUBROUTINE QMSREC
CKEY RUN /INTERNAL
C----------------------------------------------------------------------
C! called for unknown record types
C called from QMREAD
C                                         Author: E.Blucher  10.09.89
C----------------------------------------------------------------------
      CALL QUSREC
   90 CONTINUE
      END
