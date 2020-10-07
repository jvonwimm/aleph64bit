      SUBROUTINE AUCIRL(XC,YC,R,SL,B,XMIN,XMAX,PSINT,MINT)
C--------------------------------------------------------------------
C!  Compute the points of intersection of a circle with a line segment
C
C  Calls:  None
C
C  Input:   PASSED:  --XC,    X-coord of center of circle
C                    --YC,    Y-coord of center of circle
C                    --R,     radius of circle
C                    --SL,    slope of line segment
C                    --B,     Y-intercept of line segment
C                    --XMIN,  X-lower-limit of line segment
C                    --XMAX,  X-upper-limit of line segment
C
C  Output:  PASSED:  --PSINT, angle (measured counter-clockwise from
C                             X-axis) around circle at points of
C                             intersection ( in interval from 0 to 2pi)
C                    --MINT,  number of intersections
C  D. DeMille
C
C--------------------------------------------------------------------
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER(CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
C  Additional constants for TPCSIM
C  Units -- Mev,Joules,deg Kelvin,Coulombs
C
      REAL ELMASS,CROOT2,CKBOLT,CROOMT,ECHARG
      PARAMETER (ELMASS = 0.511)
      PARAMETER (CROOT2 = 1.41421356)
      PARAMETER (CKBOLT = 1.380662E-23)
      PARAMETER (CROOMT = 300.)
      PARAMETER (ECHARG = 1.602189E-19)
C
      DIMENSION PSINT(2),XTMP(2),YTMP(2)
C
C  Find the intersection points of the circle centered at (xc,yc)
C  with radius r and the line y = sl*x + b.
C
      MINT = 0
C
      A1 = 1. + SL*SL
      A2 = XC + SL*(YC - B)
      A3 = XC*XC + (YC-B)*(YC-B) - R*R
      DSQ = A2*A2 - A1*A3
C
      IF ( DSQ .LT. 0. ) THEN
         RETURN
C
      ELSEIF ( DSQ .EQ. 0. ) THEN
         NTMP = 1
         XTMP(1) = A2/A1
         YTMP(1) = SL*XTMP(1) + B
C
      ELSEIF ( DSQ .GT. 0. ) THEN
         NTMP = 2
         D = SQRT(DSQ)
         XTMP(1) = ( A2 + D )/ A1
         XTMP(2) = ( A2 - D )/ A1
         YTMP(1) = SL*XTMP(1) + B
         YTMP(2) = SL*XTMP(2) + B
C
      ENDIF
C
C  Check to see if the intersection points lie within the bounds
C  of the line segment
C
      DO 1 K = 1,NTMP
C
         IF ( XTMP(K) .GE. XMIN .AND. XTMP(K) .LE. XMAX ) THEN
C
            MINT = MINT + 1
            XDIFF = XTMP(K) - XC
            YDIFF = YTMP(K) - YC
            PSINT(MINT) = ATAN2( YDIFF,XDIFF )
            IF ( PSINT(MINT) .LT. 0.0 )
     *         PSINT(MINT) = PSINT(MINT) + TWOPI
C
         ENDIF
C
 1    CONTINUE
C
      RETURN
      END
