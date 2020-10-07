      SUBROUTINE AULLEN(SLONG,V,VOUT)
C----------------------------------------------------------------------
C J. Hilgart 10/12/86
C!give new position and direction cosines of a straight
C track, given the length of track and initial position and direction
C cosines.
C
C      INPUT:  V(1-6)    x0, y0, z0, dc1, dc2, dc3,
C               SLONG
C
C      OUTPUT: VOUT(1-6) XOUT,YOUT,ZOUT,DC1OUT,DC2OUT,DC3OUT
C --------------------------------------------------------------------
#ifndef DOC
      SAVE
C
      REAL V(6),VOUT(6)
      DO 1 I = 1, 3
         VOUT(I) = V(I) + SLONG*V(3+I)
         VOUT(I+3) = V(I+3)
  1   CONTINUE
C
      RETURN
      END
#endif
