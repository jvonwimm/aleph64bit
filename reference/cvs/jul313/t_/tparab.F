      SUBROUTINE TPARAB(TOFF,IPH,TMAX,PHMAX)
C
C----------------------------------------------------------------------
C! Parabolic fit of peak of a subpulse
C!
C!    Author:    R. Johnson    6-01-87
C!    Modified:  R. Johnson    6-01-87
C!
C!    Input:
C!        - TOFF     /R     Time at center of second pulse
C!        - IPH(3)   /I     Array of pulse heights
C!    Output:
C!        - TMAX     /R     Time at peak of parabola
C!        - PHMAX    /R     Peak height of parabola
C!
C!    Called by TPLANA
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION IPH(*)
C
      A=FLOAT(IPH(2))
      C=0.5*(FLOAT(IPH(1)+IPH(3))-A-A)
      IF (C.EQ.0.) THEN
        TMAX=TOFF
        PHMAX=A
      ELSE IF (C.GT.0.) THEN
        CALL RERROR('TPARAB',1,'Center sample is not a maximum.')
        TMAX=TOFF
        PHMAX=A
      ELSE
        B=0.5*FLOAT(IPH(3)-IPH(1))
        EXTR=-B/2./C
        TMAX=TOFF+EXTR
        PHMAX=A+(C*EXTR+B)*EXTR
      ENDIF
      RETURN
      END
#endif
