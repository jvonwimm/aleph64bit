      SUBROUTINE YVSPRM(PAR,DU,EU,DV,EV,UVCR,PMX)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C! Finds maximum and error at peak of 6-parameter paraboloid
C     REPRESENTING 2-D LIKELIHOOD PEAK
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C     F=PAR(1)*DU*DU + PAR(2)*DU + PAR(3) +
C       PAR(4)*DV*DV + PAR(5)*DV + PAR(6)*DU*DV
C  Output Arguments :
C  *  DU,DV STEPS TO MAX IN BIN UNITS
C  *  EU,EV ARE ERROR IN BIN UNITS
C  *  UVCR IS NORMALIZED CORRELATION
C  *  PMX IS INTERPOLATED VALUE AT MAXIMUM
C        IF PROBLEMS LIKE NO MAX, NOT POS-DEFINITE,
C        EU AND/OR EV WILL BE NEGATIVE
C        DU AND/OR DV MAY BE ZERO
C
C ----------------------------------------------------------------------
      DIMENSION PAR(6)
C ----------------------------------------------------------------------
      A=PAR(1)
      B=PAR(2)
      C=PAR(3)
      D=PAR(4)
      E=PAR(5)
      F=PAR(6)
C PATHOLOGICAL CASES FIRST
      IF (A .GE. 0. .AND. D .GE. 0.) THEN
C TAKE BIN CENTER, NEGATIVE ERRORS
        DU=0.
        DV=0.
        EU=-1.
        EV=-1.
        UVCR=0.
      ELSEIF (A .GE. 0.) THEN
C SOLVE IN V ONLY, NULL IN U
        DV=-.5*E/D
        EV=-SQRT(-.5/D)
        DU=0.
        EU=-1.
        UVCR=0.
      ELSEIF (D .GE. 0.) THEN
C SOLVE IN U ONLY, NULL IN V
        DU=-.5*B/A
        EU=-SQRT(-.5/A)
        DV=0.
        EV=-1.
        UVCR=0.
      ELSE
C FIND AND CHECK DETERMINANT
        DET=4.*A*D-F*F
        IF (DET .LE. 0.) THEN
C TAKE HALF-STEP IN BOTH U AND V
          DU=-.25*B/A
          DV=-.25*E/D
C USE NEGATIVE INDEPENDENT ERRORS
          EU=-SQRT(-.5/A)
          EV=-SQRT(-.5/D)
C COMPLETELY CORRELATED
          UVCR=SIGN(.99,F)
        ELSE
C THIS IS THE CASE WE HOPE FOR!
          IERR=0
          RDET=1./DET
C FIND DISTANCE TO PARABOLA MAXIMUM
          DU=RDET*(-2.*B*D+E*F)
          DV=RDET*(-2.*A*E+B*F)
C ERROR FROM CURVATURE
          EU=SQRT(-2.*D*RDET)
          EV=SQRT(-2.*A*RDET)
          UVCR=F/SQRT(4.*A*D)
        ENDIF
      ENDIF
C VALUE AT PEAK
      PMX=A*DU*DU+B*DU+C+D*DV*DV+E*DV+F*DU*DV
      RETURN
      END
