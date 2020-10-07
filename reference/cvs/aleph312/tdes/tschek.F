      SUBROUTINE TSCHEK(ISLOT,PS,S1,DS,NMX,NX,PX,ITP)
C
C----------------------------------------------------------------------
C! Check if track falls within sector
C!
C!    Author:   R. Johnson   06-08-87
C!    Modified: R. Johnson   21-10-87
C!
C!    Call by: TFNDXP
C!    Input:
C!       - ISLOT      /I     TPC sector slot number (1-36)
C!       - PS(*)      /R     Track helix parameters:
C!                           1/r,tanl,phi0,d0,z0
C!                           These must be in the coordinate system
C!                           of the sector ISLOT:  z=0 is at the
C!                           sense wire plane with the positive z
C!                           axis pointing toward detector center.
C!                           The origin is at the padrow centers, and
C!                           the x axis divides the sector into two
C!                           symmetric pieces.
C!       - S1         /R     Distance to starting point on the helix
C!       - DS         /R     Length of arc to consider on the helix
C!       - NMX        /I     Maximum number of intersections to find
C!    Output:
C!       - NX         /I     Number of intersections with sector boundar
C!                           plus number of endpoints within sector.
C!       - PX(3, )    /R     Coordinates of intersection point or
C!                           points.
C!       - ITP( )     /I     Type of intersection
C!                               1= enter or exit from edge
C!                               2= enter or exit from end
C!                               3= termination within the sector
C!
C!    Description:
C!    -----------
C!    This routine returns all the intersection points of a circular
C!    track element with the boundaries of a TPC sector specified by
C!    ISLOT.  If the element begins or ends within the sector, then
C!    that point also is included.  It is only approximate for end
C!    crossings and internal points.
C!
C!----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeow.h"
C
      DIMENSION PS(*),PX(3,*),ITP(*)
C
C     NS  = number of line segments for the sector outline
C     XS  = starting point (x,y) for each line segment
C     EXS = minimum and maximum x values for each segment
C     EYS = minimum and maximum y values for each segment
C     TS  = direction cosines (tx,ty) for each line segment
C     DLS = length of each line segment
C     AS  = x intercept of each line segment
C     BS  = inverse slope of each line segment
C     IORDR = order in which to check the line segments
C
      PARAMETER (LMXSEG=2*LTCORN)
      DIMENSION NS(LTSTYP),XS(2,LMXSEG,LTSTYP),EXS(2,LMXSEG,LTSTYP),
     &          EYS(2,LMXSEG,LTSTYP),TS(2,LMXSEG,LTSTYP),
     &          DLS(LMXSEG,LTSTYP),AS(LMXSEG,LTSTYP),BS(LMXSEG,LTSTYP)
      DIMENSION XC(2),IORDR(LMXSEG,LTSTYP),BV(2)
      DIMENSION SEND(2),ZEND(2),RMIN(LTSTYP),RMAX(LTSTYP)
      DIMENSION PMIN(LTSTYP),PMAX(LTSTYP),ZTEND(2)
      DIMENSION XMAX(LTSTYP),XMIN(LTSTYP),YMIN(LTSTYP),YMAX(LTSTYP)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA IORDR/8,6,2,4,3,5,7,1,9,10,11,12,
     &           10,1,9,3,7,4,6,5,2,8,11,12,
     &           10,1,9,3,7,4,6,5,2,8,11,12/
      DATA EPS/0.00001/
C
C++   On the first call, calculate parameters for each border segment.
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        DO 50 ISTYP=1,LTSTYP
C
C++       Store all the corner positions (TPGEOM contains only half the
C++       corners; reflect about the x axis to get the other half).
C
          DO 40 IC=1,NTPCRN(ISTYP)
            XS(1,IC,ISTYP)=TPCORN(1,IC,ISTYP)
            XS(2,IC,ISTYP)=TPCORN(2,IC,ISTYP)
   40     CONTINUE
          DO 42 I=1,NTPCRN(ISTYP)
            IC=NTPCRN(ISTYP)+1-I
            XS(1,I+NTPCRN(ISTYP),ISTYP)=TPCORN(1,IC,ISTYP)
            XS(2,I+NTPCRN(ISTYP),ISTYP)=-TPCORN(2,IC,ISTYP)
   42     CONTINUE
C
C++       Find upper limit of distance of interior point from any edge
C
          XMIN(ISTYP)=999.
          XMAX(ISTYP)=0.
          YMIN(ISTYP)=999.
          YMAX(ISTYP)=0.
          RMIN(ISTYP)=999.
          RMAX(ISTYP)=0.
          PMIN(ISTYP)=TWOPI
          PMAX(ISTYP)=0.
          NS(ISTYP)=2*NTPCRN(ISTYP)
          DO 43 I=1,NS(ISTYP)
            XMIN(ISTYP)=AMIN1(XMIN(ISTYP),XS(1,I,ISTYP))
            XMAX(ISTYP)=AMAX1(XMAX(ISTYP),XS(1,I,ISTYP))
            YMIN(ISTYP)=AMIN1(YMIN(ISTYP),XS(2,I,ISTYP))
            YMAX(ISTYP)=AMAX1(YMAX(ISTYP),XS(2,I,ISTYP))
            R= XS(1,I,ISTYP)**2 + XS(2,I,ISTYP)**2
            PHI=ATAN2(XS(2,I,ISTYP),XS(1,I,ISTYP))
            RMIN(ISTYP)=AMIN1(RMIN(ISTYP),R)
            RMAX(ISTYP)=AMAX1(RMAX(ISTYP),R)
            PMIN(ISTYP)=AMIN1(PMIN(ISTYP),PHI)
            PMAX(ISTYP)=AMAX1(PMAX(ISTYP),PHI)
   43     CONTINUE
C
C++       Now, find parameters of a line segment for each corner.
C
          DO 47 IC=1,NS(ISTYP)
            IF (IC.EQ.NS(ISTYP)) THEN
              INXT=1
            ELSE
              INXT=IC+1
            ENDIF
            ES1=XS(1,INXT,ISTYP)
            ES2=XS(2,INXT,ISTYP)
            EXS(1,IC,ISTYP)=AMIN1(ES1,XS(1,IC,ISTYP))
            EXS(2,IC,ISTYP)=AMAX1(ES1,XS(1,IC,ISTYP))
            EYS(1,IC,ISTYP)=AMIN1(ES2,XS(2,IC,ISTYP))
            EYS(2,IC,ISTYP)=AMAX1(ES2,XS(2,IC,ISTYP))
            DLS(IC,ISTYP)=SQRT((ES1-XS(1,IC,ISTYP))**2
     &                      + (ES2-XS(2,IC,ISTYP))**2)
            TS(1,IC,ISTYP)=(ES1-XS(1,IC,ISTYP)) / DLS(IC,ISTYP)
            TS(2,IC,ISTYP)=(ES2-XS(2,IC,ISTYP)) / DLS(IC,ISTYP)
            BS(IC,ISTYP)=TS(1,IC,ISTYP)/TS(2,IC,ISTYP)
            AS(IC,ISTYP)=XS(1,IC,ISTYP)-BS(IC,ISTYP)*XS(2,IC,ISTYP)
   47     CONTINUE
   50   CONTINUE
      ENDIF
C
C++   Find the center of the track circle
C
      R=1./PS(1)
      R2=R**2
      SPHI0=SIN(PS(3))
      CPHI0=COS(PS(3))
      XC(1)=-(R-PS(4))*SPHI0
      XC(2)= (R-PS(4))*CPHI0
      SGN=SIGN(1.,R)
      R=ABS(R)
C
C++   Get the COSINE and TANGENT of the dip angle
C
      TANL=PS(2)
      COSL=1./SQRT(1.+TANL**2)
      SINL=COSL*TANL
C
C++   Check whether the beginning and/or end of the segment are
C++   within the sector.  This is not exact--we will pick up some
C++   points outside the sector.
C
      NX=0
      ISTYP=ITPTYP(ISLOT)
      SEND(1)=S1
      SEND(2)=S1+DS
      DO 568 IEND=1,2
        ZEND(IEND)= PS(5) + SEND(IEND)*SINL
        IF (ZEND(IEND).LT.0. .OR. ZEND(IEND).GT.ZTPCMX) GO TO 568
        ALPHA=SEND(IEND)*COSL/R
        THETA= PS(3) - SGN*(PIBY2-ALPHA)
        XEND= XC(1) + R*COS(THETA)
        IF (XEND.LT.XMIN(ISTYP)) GO TO 568
        IF (XEND.GT.XMAX(ISTYP)) GO TO 568
        YEND= XC(2) + R*SIN(THETA)
        IF (YEND.LT.YMIN(ISTYP)) GO TO 568
        IF (YEND.GT.YMAX(ISTYP)) GO TO 568
        RP= XEND**2 + YEND**2
        IF (RP.LT.RMIN(ISTYP)) GO TO 568
        IF (RP.GT.RMAX(ISTYP)) GO TO 568
        PHIP=ATAN2(YEND,XEND)
        IF (PHIP.LT.PMIN(ISTYP)) GO TO 568
        IF (PHIP.GT.PMAX(ISTYP)) GO TO 568
        NX=NX+1
        PX(1,NX)=XEND
        PX(2,NX)=YEND
        PX(3,NX)=ZEND(IEND)
        ITP(NX)=3
        IF (NX.GE.NMX) GO TO 999
  568 CONTINUE
C
C++   Quit immediately if both ends of the track segment are outside
C++   the z limits of the sector.
C
      IF (ZEND(1).GT.ZTPCMX .AND. ZEND(2).GT.ZTPCMX) THEN
        GO TO 999
      ENDIF
      IF (ZEND(1).LT.0. .AND. ZEND(2).LT.0.) THEN
        GO TO 999
      ENDIF
C
C++   Check for tracks entering the sector from the ends.  This also
C++   is not exact--we will get some crossing the endplanes a little
C++   bit outside the sector.
C
      NEND=0
      IF (ZEND(1).LT.0. .OR. ZEND(2).LT.0.) THEN
        NEND=NEND+1
        ZTEND(NEND)=0.
      ENDIF
      IF (ZEND(1).GT.ZTPCMX .OR. ZEND(2).GT.ZTPCMX) THEN
        NEND=NEND+1
        ZTEND(NEND)=ZTPCMX
      ENDIF
      DO 793 IEND=1,NEND
        IF (TANL.NE.0.) THEN
          ARC= (ZTEND(IEND)-PS(5))/SINL
          IF (ARC.GT.SEND(1) .AND. ARC.LT.SEND(2)) THEN
            ALPHA= ARC*COSL/R
            THETA= PS(3) - SGN*(PIBY2-ALPHA)
            XEND= XC(1) + R*COS(THETA)
            IF (XEND.LT.XMIN(ISTYP)) GO TO 793
            IF (XEND.GT.XMAX(ISTYP)) GO TO 793
            YEND= XC(2) + R*SIN(THETA)
            IF (YEND.LT.YMIN(ISTYP)) GO TO 793
            IF (YEND.GT.YMAX(ISTYP)) GO TO 793
            RP= XEND**2 + YEND**2
            IF (RP.LT.RMIN(ISTYP)) GO TO 793
            IF (RP.GT.RMAX(ISTYP)) GO TO 793
            PHIP=ATAN2(YEND,XEND)
            IF (PHIP.LT.PMIN(ISTYP)) GO TO 793
            IF (PHIP.GT.PMAX(ISTYP)) GO TO 793
            NX=NX+1
            PX(1,NX)=XEND
            PX(2,NX)=YEND
            PX(3,NX)=ZTEND(IEND)
            ITP(NX)=2
            IF (NX.GE.NMX) GO TO 999
          ENDIF
        ENDIF
  793 CONTINUE
C
C++   Loop over edges and check for intersections.
C
      DO 200 II=1,NS(ISTYP)
        IS=IORDR(II,ISTYP)
C
C++     Does the circle cross the line in question?
C
        A1=AS(IS,ISTYP)-XC(1)
        B12=1.+BS(IS,ISTYP)**2
        BY0=BS(IS,ISTYP)*XC(2)
        Q2= B12*R2 - A1**2 - BY0*(2.*A1+BY0)
        IF (Q2.LT.0) GO TO 200
C
C++     Solve for both solutions.
C
        Q=SQRT(Q2)
        BQ=XC(2)-A1*BS(IS,ISTYP)
        DO 100 ISOL=1,3,2
          SGNS=FLOAT(ISOL-2)
C
C++       Is the solution within the bounds of the line segment?
C
          YSOL= (BQ + SGNS*Q)/B12
          IF (YSOL.LT.EYS(1,IS,ISTYP)-EPS) GO TO 100
          IF (YSOL.GT.EYS(2,IS,ISTYP)+EPS) GO TO 100
          XSOL= AS(IS,ISTYP) + BS(IS,ISTYP)*YSOL
          IF (XSOL.LT.EXS(1,IS,ISTYP)-EPS) GO TO 100
          IF (XSOL.GT.EXS(2,IS,ISTYP)+EPS) GO TO 100
C
C++       Find the angle through which the track turns from its origin
C++       to the boundary crossing point.
C
          THETA=ATAN2(YSOL-XC(2),XSOL-XC(1))
          IF (THETA.LT.0.) THETA=THETA+TWOPI
          ALPHA= PIBY2 + SGN*(THETA-PS(3))
          IF (ALPHA.LT.0.) THEN
            ALPHA=ALPHA+TWOPI
          ELSEIF (ALPHA.GT.TWOPI) THEN
            ALPHA=ALPHA-TWOPI
          ENDIF
C
C++       Find the z position of the intersection point
C
          ZSOL= PS(5) + R*ALPHA*TANL
C
C++       Is the intersection within the TPC volume?
C
          IF (ZSOL.LT.0. .OR. ZSOL.GT.ZTPCMX) GO TO 100
C
C++       Is the intersection within bounds of the track segment?
C
          S= R*ALPHA/COSL
          IF (S.LT.S1 .OR. S.GT.(S1+DS)) GO TO 100
C
C++       Store the valid intersection points
C
          NX=NX+1
          PX(1,NX)=XSOL
          PX(2,NX)=YSOL
          PX(3,NX)=ZSOL
          ITP(NX)=1
          IF (NX.GE.NMX) GO TO 999
  100   CONTINUE
  200 CONTINUE
  999 CONTINUE
      RETURN
      END
#endif
