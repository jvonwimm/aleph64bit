      SUBROUTINE EMSKEV
C=======================================================================
C!   Steering routine for mask analysis
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C?
C!======================================================================
#ifndef DOC
C
      CALL ECST(IER)
      IF(IER.EQ.0) THEN
C
C    calculate E-Flow
C
        CALL FLOWFI (IERR)
        CALL EFAXE  (IERR)
      ENDIF
      RETURN
      END
#endif
