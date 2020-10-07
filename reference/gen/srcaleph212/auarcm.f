      FUNCTION AUARCM (A)
C.
C...AUARCM  1.00  850101                             R.Beuselinck
C.
CKEY COMPUTE
C.
C!  Compute shortest (signed) phi distance around an arc.
C.
C.  Input:
C.          - A   /R  : Difference of two phi values, (A2 - A1).
C.                      A1, A2 must both be in the range 0 to 2pi.
C.  Result:
C.          - AUARCM /R  : Shortest phi displacement (with sign) to
C.                         go from A1 to A2 around an arc.
C.
C.  This routine is a copy of the TASSO routine ARCMIN.
C.  This function calculates the minimum distance apart in phi with
C.  the appropriate sign of two phi values whose difference is the
C.  calling argument of this routine.  These two values must both be
C.  in the range 0 to 2pi.
C.  E.G. AUARCM(B-A) gives the shortest arc round a circle from A to B.
C.  This routine takes care of the 0-2pi wrap-around problem.
C.
C-----------------------------------------------------------------------
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (FIVPI = 5.*PI)
C
      AUARCM=A
      IF (ABS(A).LE.PI) GOTO 2
      AUARCM=AMOD(A+FIVPI,TWOPI)-PI
2     RETURN
      END
