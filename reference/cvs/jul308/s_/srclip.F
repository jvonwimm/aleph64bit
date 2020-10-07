      SUBROUTINE SRCLIP(NC,XC,YC,R,PHI,IS)
C----------------------------------------------------------------------
C! Clip polygon at straight line
C!
C!    Author:     H. Meinhard       08-Jan-1988
C!    Modified:   H. Meinhard       31-May-1989  (2)
C!
C!    Input:      - NC        /I    no of corners of polygon [3,8]
C!                - XCi       /F    x-coordinates of corners [-.1,.1]
C!                - YCi       /F    y-coordinates of corners [-.1,.1]
C!                - R         /F    distance of straight line from
C!                                  origin [0.,.1]
C!                - PHI       /F    azimuth of distance [0.,6.29]
C!                - IS        /I    side to suppress [1,2]
C!                                  1 = beyond origin, 2 = origin incl.
C!    Output:     - NC        /I    no of corners of clipped pol. [3,8]
C!                - XCi       /F    see above
C!                - YCi       /F    see above
C?
C!======================================================================
#ifndef DOC
      PARAMETER (NCMAX=10)
      REAL XC(*),YC(*),XCBAK(NCMAX),YCBAK(NCMAX)
      LOGICAL SRLVAL,LVFST,LVCUR,LVBAK
      EXTERNAL SRLVAL
C----------------------------------------------------------------------
C copy polyline to backup buffer
      NCBAK = NC
      NC = 0
      DO 300 I = 1, NCBAK
        XCBAK(I) = XC(I)
        YCBAK(I) = YC(I)
  300 CONTINUE
C
C check first point; if point is valid, keep it
      LVCUR = SRLVAL(XCBAK(1),YCBAK(1),R,PHI,IS)
      LVFST = LVCUR
      IF (LVCUR) THEN
        NC = 1
        XC(1) = XCBAK(1)
        YC(1) = YCBAK(1)
      ENDIF
C
C loop over points (2...NCBAK)
      DO 310 IP = 2, NCBAK
        LVBAK = LVCUR
        LVCUR = SRLVAL(XCBAK(IP),YCBAK(IP),R,PHI,IS)
C
C if both points are valid, keep current point
        IF (LVBAK .AND. LVCUR) THEN
          NC = NC+1
          XC(NC) = XCBAK(IP)
          YC(NC) = YCBAK(IP)
C
C if old point valid, new point invalid, get intersection point of
C polygon edge with clipping straight line, keep intersection point;
C if old point invalid, new point valid, get intersection point,
C keep both intersection point and current point
        ELSE IF (LVBAK .NEQV. LVCUR) THEN
          CALL SRINTS(XCBAK(IP-1),YCBAK(IP-1),XCBAK(IP),YCBAK(IP),
     +                R,PHI,XS,YS)
          NC = NC+1
          XC(NC) = XS
          YC(NC) = YS
          IF ((.NOT. LVBAK) .AND. LVCUR) THEN
            NC = NC+1
            XC(NC) = XCBAK(IP)
            YC(NC) = YCBAK(IP)
          ENDIF
C
C if old point invalid, new point invalid, do nothing
        ENDIF
C
C bottom of loop over points
  310 CONTINUE
C
C check the edge connecting points NCBAK and 1; if one is valid, the
C other one invalid, keep intersection point of edge with clip line
      IF (LVFST .NEQV. LVCUR) THEN
        CALL SRINTS(XCBAK(1),YCBAK(1),XCBAK(NCBAK),YCBAK(NCBAK),
     +              R,PHI,XS,YS)
        NC = NC+1
        XC(NC) = XS
        YC(NC) = YS
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
