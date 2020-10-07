      SUBROUTINE YVSTP0(XYZ,DIR,CRV,S0,IERR)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C! Finds path-length that steps track vector to z=0
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  XYZ(3) IS TRACK POSITION  AT S=0
C  *  DIR(3) IS TRACK DIRECTION AT S=0
C  *  CRV(3) IS TRACK CURVATURE AT S=0
C       COMPONENTS 1,2 ARE IGNORED
C  Output Arguments :
C  *  S0 IS S VALUE THAT MAKES XYZS(3)=0
C  *  IERR IS ERROR FLAG
C       =0 FOR OK
C       =1 IF TRACK CURVES AWAY
C          (S0 IS CALCULATED WITH CRV(3)=0 IN THIS CASE)
C       =2 IF DIR(3)=CRV(3)=0
C
C ----------------------------------------------------------------------
      DIMENSION XYZ(3),DIR(3),CRV(3)
      REAL*8 B2,AC4,RAD
C ----------------------------------------------------------------------
C
      A=.5*CRV(3)
      B=DIR(3)
      C=XYZ(3)
C DO FIRST WITH NO CURVATURE AS BACKUP
      IF (B .NE. 0.) THEN
        S0=-C/B
        IERR=0
      ELSE
        IERR=2
      ENDIF
C TRY WITH CURVATURE
      IF (A .NE. 0.) THEN
C A LITTLE PRECISION GYMNASTICS
        B2=B
        B2=B2*B2
        AC4=A
        AC4=AC4*C*4.D0
        RAD=B2-AC4
        IF (RAD .GE. 0.) THEN
          IERR=0
          RAD=DSQRT(RAD)
C CHOOSE SOLUTION WITH SMALLER PATH-LENGTH TO Z=0
          T1=-B+RAD
          T2=-B-RAD
          IF (ABS(T1) .LT. ABS(T2)) THEN
            S0=.5*T1/A
          ELSE
            S0=.5*T2/A
          ENDIF
        ELSE
C CURVES AWAY FROM Z=0 PLANE
C LEAVE THE NO-CURVATURE RESULT
          IERR=1
        ENDIF
      ENDIF
C
      RETURN
      END
