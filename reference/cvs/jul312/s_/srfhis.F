      SUBROUTINE SRFHIS(XHIS,YHIS,N,YLOW,YUPP,YREF,MODE,XREF,IER)
C----------------------------------------------------------------------
C! fit straight line to histogram
C!
C!    Author:     H. Meinhard       27-Jun-1989
C!
C!    Input:      - XHISn     /F    abscissas of histogram segment
C!                - YHISn     /F    ordinates of histogram segment
C!                - N         /I    number of channels in segment
C!                - YLOW      /F    mimimum ordinate to be used in fit
C!                - YUPP      /F    maximum ordinate to be used in fit
C!                - YREF      /F    ordinate for which abscissa must be
C!                                  calculated
C!                - MODE      /I    mode of histogram segment:
C!                                  0 = raising edge
C!                                  1 = falling edge
C!    Output:     - XREF      /F    abscissa that YREF refers to
C!                - IER       /I    error code:
C!                                  0 = no error
C!                                  1 = no starting point for fit
C!                                  2 = not enough points for fit
C!                                  3 = straight line fit failed
C!                                  4 = mode not defined
C!
C!    Description
C!    ===========
C!    Given a segment of a histogram that contains either a raising
C!    or a falling edge, fit a straight line to the subset of the
C!    segment that has at least YLOW entries and at most YUPP entries.
C!    For a given reference of the ordinate, return the abscissa that
C!    corresponds to that ordinate.
C----------------------------------------------------------------------
#ifndef DOC
      REAL XHIS(*),YHIS(*)
C----------------------------------------------------------------------
      IF (MODE .NE. 0 .AND. MODE .NE. 1) THEN
        IER = 4
        GOTO 999
      ENDIF
C
C search first channel to be used for the fit
      K1 = 0
      DO 300 I = 1, N
        IF ((MODE .EQ. 0 .AND. YHIS(I) .GT. YLOW) .OR.
     +      (MODE .EQ. 1 .AND. YHIS(I) .LT. YUPP)) THEN
          K1 = I
          GOTO 310
        ENDIF
  300 CONTINUE
  310 CONTINUE
      IF (K1 .LE. 0) THEN
        IER = 1
        GOTO 999
      ENDIF
C
C search last channel to be used for the fit
      K2 = 0
      DO 320 I = N, K1, -1
        IF ((MODE .EQ. 0 .AND. YHIS(I) .LT. YUPP) .OR.
     +      (MODE .EQ. 1 .AND. YHIS(I) .GT. YLOW)) THEN
          K2 = I
          GOTO 330
        ENDIF
  320 CONTINUE
  330 CONTINUE
C
C number of points for fit
      NFIT = K2 - K1 + 1
      IF (NFIT .LT. 2) THEN
        IER = 2
        GOTO 999
      ENDIF
C
C straight line fit
      IFAIL = 0
      CALL LLSQ(NFIT,XHIS(K1),YHIS(K1),A1,A2,IFAIL)
      IF (IFAIL .NE. 0 .OR. A2 .EQ. 0.) THEN
        IER = 3
        GOTO 999
      ENDIF
C
C abscissa
      XREF = (YREF - A1) / A2
C
  999 CONTINUE
      RETURN
      END
#endif
