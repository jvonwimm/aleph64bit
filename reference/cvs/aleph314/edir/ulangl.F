      FUNCTION ULANGL(X,Y)
C-----------------------------------------------------------------------
CKEY EDIR
C! Routine not documented
C-
C   Input  :
C   Output :
C-
C                                    Author: J.C.Brient - 910400
C-----------------------------------------------------------------------
#ifndef DOC
      ULANGL=0.
C --
C   reconstruct the angle from x and y coordinate
C --
      R=SQRT(X**2+Y**2)
      IF(R.LT.1E-20) RETURN
      IF(ABS(X)/R.LT.0.8) THEN
        ULANGL=SIGN(ACOS(X/R),Y)
      ELSE
        ULANGL=ASIN(Y/R)
        IF(X.LT.0..AND.ULANGL.GE.0.) THEN
          ULANGL=3.1415927-ULANGL
        ELSEIF(X.LT.0.) THEN
          ULANGL=-3.1415927-ULANGL
        ENDIF
      ENDIF
      RETURN
      END
#endif
