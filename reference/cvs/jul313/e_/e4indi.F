      SUBROUTINE E4INDI(R,J1,J2)
C----------------------------------------------------------------------
C!  - R2 ESTIMATOR : REORDER I J INDICES OF 2 STOREYS
C!
C!    AUTHOR  : D. PALLIN
C!    MODIFIED:
C?
C!======================================================================
#ifndef DOC
      J2=INT(R)
      J1=J2-1
      IF(R-INT(R).GT..5)THEN
         J1=J2
         J2=J1+1
         ENDIF
      RETURN
      END
#endif
