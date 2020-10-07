      SUBROUTINE SRIHIS(X0,DELX,N,YHIS,X,Y,IER)
C----------------------------------------------------------------------
C! Interpolation in histogram
C!
C!    Author:     H. Meinhard       27-Jun-1989
C!
C!    Input:      - X0        /F    start value of histogram abscissa
C!                - DELX      /F    step of histogram abscissa
C!                - N         /I    number of histogram channels
C!                - YHISn     /F    array containing channel contents
C!                - X         /F    abscissa where interpolation is
C!                                  required
C!    Output:     - Y         /F    interpolated ordinate value
C!                - IER       /I    error code: 0 = no error
C!                                              1 = X out of range
C!
C!    Description
C!    ===========
C!    This routine gives the interpolated histogram value for a given
C!    abscissa.
C?
C!======================================================================
#ifndef DOC
#ifndef DOC
      REAL YHIS(*)
C----------------------------------------------------------------------
      A = (X-X0) / DELX + .5
      IF (A .LT. 1. .OR. A .GT. REAL(N)) THEN
        IER = 1
        GOTO 999
      ENDIF
      I = INT(A)
      A = A - REAL(I)
      Y = (1.-A)*YHIS(I) + A*YHIS(I+1)
  999 CONTINUE
      RETURN
      END
#endif
#endif
