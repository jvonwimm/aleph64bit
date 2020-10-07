      SUBROUTINE SRINTS(X1,Y1,X2,Y2,R,PHI,XS,YS)
C----------------------------------------------------------------------
C! Get intersection point
C!
C!    Author:     H. Meinhard       08-Jan-1988
C!
C!    Input:      - Xi,Yi     /F    coordinates of points delimiting
C!                                  the polygon edge [-.1,.1]
C!                - R         /F    distance of straight line from
C!                                  origin [0.,.1]
C!                - PHI       /F    azimuth of distance [0.,6.29]
C!    Output:     - XS,YS     /F    coordinates of intersection point
C!                                  [-.1,.1]
C!
C!    Description
C!    ===========
C!    Determine the intersection point of a polygon edge (given by car-
C!    tesian coordinates) with the clipping straight line (given by
C!    r and phi of nearest approach to origin)
C?
C!======================================================================
#ifndef DOC
      DENOM = (X2-X1)*COS(PHI) + (Y2-Y1)*SIN(PHI)
      IF (ABS(DENOM) .GT. 1.E-12) THEN
        XS = (R*(X2-X1)+(X1*Y2-X2*Y1)*SIN(PHI))/DENOM
        YS = (R*(Y2-Y1)-(X1*Y2-X2*Y1)*COS(PHI))/DENOM
      ELSE
        XS = 999999.
        YS = 999999.
      ENDIF
      RETURN
      END
#endif
