      LOGICAL FUNCTION SRLVAL(X,Y,R,PHI,IS)
C----------------------------------------------------------------------
C! Check validity of point
C!
C!    Author:     H. Meinhard       08-Jan-1988
C!
C!    Input:      - X,Y       /F    coordinates of point to be checked
C!                                  [-.1,.1]
C!                - R         /F    distance of clip line from origin
C!                                  [-.1,.1]
C!                - PHI       /F    azimuth of distance [0.,6.29]
C!                - IS        /I    side to suppress [1,2]
C!                                  1 = beyond origin, 2 = origin incl.
C!    Output:     - SRLVAL    /L    .TRUE. if point is kept
C!
C!    Description
C!    ===========
C!    Check whether point (x,y) is kept while clipping at a straight
C!    line with distance r and distance azimuth phi
C!    Do this by getting the difference vector between point to be
C!    checked and base point of the distance on the straight line, and
C!    looking at the projection of the difference vector onto the
C!    distance direction
C?
C!======================================================================
#ifndef DOC
      SRLVAL = .TRUE.
      DIST = X*COS(PHI) + Y*SIN(PHI) - R
      IF (IS .EQ. 1 .AND. DIST .GT. 0.) SRLVAL = .FALSE.
      IF (IS .EQ. 2 .AND. DIST .LT. 0.) SRLVAL = .FALSE.
      RETURN
      END
#endif
