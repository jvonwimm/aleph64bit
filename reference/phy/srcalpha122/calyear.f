      SUBROUTINE CALYEAR(NRUN)
C ---------------------------------------------------------------------
C - Author: M.N Minard          940121
C!    Computes year-dependant calibration
C     Called from ECGATI
C ---------------------------------------------------------------------
      IF(NRUN.GT.14000.AND.NRUN.LT.18000)THEN
        CALL CAL92(NRUN)
      ENDIF
      RETURN
      END
