      LOGICAL FUNCTION TSCINT(HP,ISLOT)
C
C----------------------------------------------------------------------
C! Test if helix intersects a given TPC sector in r*phi
CKEY TPCDES HELIX SECTOR INTERSECTION / USER
C
C  R. Johnson  28-11-90
C  Modified     8-07-97 M.Cattaneo
C     Protect against infinite momentum tracks (e.g. if magnetic field is off) 
C  Modified    30-07-97 A.Waananen
C     Add 201 statement after end of DO loop for Linux
C
C  Input:         HP      /R      Helix parameters (as in FRFT)
C                 ISLOT   /I      TPC sector number (1-36)
C  Output:        TSCINT  /L      TRUE if the track intersects
C                                 at least one edge of the sector in
C                                 the x-y plane AND the intersection
C                                 point in z is between 0 and 220cm
C                                 in the sector coordinates
C
C  Only the first 180 degrees of the helix are considered.
C  Note that alignment corrections are ignored and that a track can
C  pass through the sector without TSCINT=.TRUE. if it enters through
C  the membrane and exits through the endplate or vice-versa.
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
C
      DIMENSION HP(*),PS(5)
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
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA IORDR/8,6,2,4,3,5,7,1,9,10,11,12,
     &           10,1,9,3,7,4,6,5,2,8,11,12,
     &           10,1,9,3,7,4,6,5,2,8,11,12/
      DATA EPS/0.00001/
C
C Skip if track has infinite momentum (B-field off?)
      IF( HP(1) .EQ. 0. ) GOTO 201
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
C++       Now, find parameters of a line segment for each corner.
C
          NS(ISTYP)=2*NTPCRN(ISTYP)
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
C++   Transform the helix parameters to the sector system.  No alignment
C++   corrections are made here.
C
      CALL TGHPTS(ISLOT,HP,PS)
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
C++   The starting point and length of the helix segment
C
      S1=0.
      S2=ABS(R)*PI
C
C++   Loop over edges and check for intersections.
C
      ISTYP=ITPTYP(ISLOT)
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
          IF (S.LT.S1 .OR. S.GT.S2) GO TO 100
C
C++       We found a valid intersection, so exit immediately
C
          TSCINT=.TRUE.
          RETURN
  100   CONTINUE
  200 CONTINUE
  201 CONTINUE
      TSCINT=.FALSE.
      RETURN
      END
#endif
