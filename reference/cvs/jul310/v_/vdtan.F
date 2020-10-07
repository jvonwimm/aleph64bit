      SUBROUTINE VDTAN(IV,JFAC,XIN,XOUT,TNG)
C ----------------------------------------------------------------------
C!  For a given VDHT track entry (XIN) and exit (XOUT) and
C!  the global face number JFAC, get the cotangent of the angle
C!  between the track and wafer normal projected to the view
C!
C - Author          Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C
C  Input :   IV    - view
C            JFAC  - global face number
C            XIN   - VDHT entry
C            XOUT  - VDHY exit
C
C  Output :  TNG   - the cotangent of the angle that the track makes
C                    with the wafer, projected to the view
C
C -----------------------------------------------------------------------
#ifndef DOC
      INTEGER IV,JFAC
      REAL XIN(3), XOUT(3), TNG
      REAL VMOD, VWTHCK, VDOT
      INTEGER VPHIFN, IERR
      REAL PHIN, XDIFF(3), UVEC(2), WID, CT
C-- default value, 45 degree
      TNG = 1.0
C-- find the track element in the wafer
      CALL VSUB(XOUT,XIN,XDIFF,3)
C-- wafer thickness
      WID = VWTHCK()
      IF (WID.LE.0.0) THEN
        CALL RERROR('VDTAN',1,'Arithmatic trap, divide by zero, view=1')
        RETURN
      ENDIF
      IF (IV.EQ.1) THEN
        TNG = XDIFF(3)/WID
      ELSE
C-- get the normal to the face
        IERR = VPHIFN(JFAC,PHIN)
C-- a unit vector along u direction is (-sin cos 0)
        UVEC(1) = -SIN(PHIN)
        UVEC(2) = COS(PHIN)
        CT = VDOT(UVEC,XDIFF,2)
        TNG = CT/WID
      ENDIF
      RETURN
      END
#endif
