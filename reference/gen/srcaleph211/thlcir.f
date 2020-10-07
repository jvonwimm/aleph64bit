      SUBROUTINE THLCIR(HP,RAD,S,PHI,Z,IER)
C
C---------------------------------------------------------------------
C! Find intersection of helix with circle of radius RAD centered
C! at the origin
C!
C! Author:  R. Johnson   15-01-89
C!
C!     Input:
C!             HP      5 helix parameters
C!                           1/r,tanl,phi0,d0,z0 (world sign convention)
C!             RAD     Radius of circle
C!     Output:
C!             S       Arc length along helix to 2 intersection points
C!                     on the first turn of the helix
C!             PHI     Phi coordinates of the 2 intersection points
C!             Z       Z coordinates of the 2 intersection points
C!             IER     IER=0 if everything is OK, =1 if no intersection
C!
C---------------------------------------------------------------------
      SAVE
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
C
      DIMENSION HP(*),S(*),Z(*),PHI(*),SGNC(2)
      DATA SGNC/1.,-1./
C
      IF (HP(1).EQ.0.) THEN
        IER=1
        GO TO 999
      ENDIF
      R= 1./HP(1)
      SGN=SIGN(1.0,R)
      RHOC= (R-HP(4))*SGN
      CB= (RAD**2 - HP(4)*(2.*R-HP(4)))/(2.*RAD*RHOC)
      IF (ABS(CB).GT.1.) THEN
        IER=1
        GO TO 999
      ENDIF
      BETA=ACOS(CB)
      COSL=1./SQRT(1.+HP(2)**2)
      RA=ABS(R)
      ARG=RAD*SIN(BETA)/RA
      IF (ARG.GT.1.) ARG=1.
      IF (ARG.LT.-1.) ARG=-1.
      ALPHA=ASIN(ARG)
      IF (RAD**2 .GT. RHOC**2 + R**2) ALPHA=PI-ALPHA
      DO 400 L=1,2
        PHI(L)= HP(3)+SGN*(PIBY2-SGNC(L)*BETA)
        IF (PHI(L).LT.0.) THEN
          PHI(L)=PHI(L)+TWOPI
        ELSEIF (PHI(L).GE.TWOPI) THEN
          PHI(L)=PHI(L)-TWOPI
        ENDIF
        S(L)=ALPHA*RA/COSL
        Z(L)= HP(5)+RA*ALPHA*HP(2)
        ALPHA=TWOPI-ALPHA
  400 CONTINUE
      IER=0
C
  999 RETURN
      END
