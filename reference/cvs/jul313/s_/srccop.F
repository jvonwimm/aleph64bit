      SUBROUTINE SRCCOP(NC1,XC1,YC1,NC2,XC2,YC2)
C----------------------------------------------------------------------
C! Copy polygon 1 to polygon 2
C!
C!    Author:     H. Meinhard       27-Jan-1988
C!
C!    Input:      - NC1       /I    number of corners of old polygon
C!                                  [3,8]
C!                - XC1i,YC1i /F    coordinates of corners of old
C!                                  polygon [-.1,.1]
C!    Output:     - NC2       /I    number of corners of new polygon
C!                                  [3,8]
C!                - XC2i,YC2i /F    coordinates of corners of new
C!                                  polygon [-.1,.1]
C?
C!======================================================================
#ifndef DOC
      REAL XC1(*),YC1(*), XC2(*),YC2(*)
C----------------------------------------------------------------------
      NC2 = NC1
      DO 300 IC = 1, NC2
        XC2(IC) = XC1(IC)
        YC2(IC) = YC1(IC)
  300 CONTINUE
      RETURN
      END
#endif
