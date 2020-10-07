      SUBROUTINE AUHLEN(FIELD,CHAR,SLONG,V,VOUT)
C----------------------------------------------------------------------
C J. Hilgart 10/12/86
C!give new position and direction cosines of helical
C track, given the length of track and initial position and direction
C cosines.
C
C      Input:  FIELD     in kG
C              CHAR      particle charge
C              V(1-7)    x0, y0, z0, dc1, dc2, dc3,ptot
C              SLONG     path length
C      OUTPUT: VOUT(1-6) XOUT,YOUT,ZOUT,DC1OUT,DC2OUT,DC3OUT
C --------------------------------------------------------------------
      SAVE
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
      DIMENSION V(7),VOUT(6)
C
      CHAR = CHAR*SIGN(1.,FIELD)
      BF = ABS(FIELD)
      RAD=V(7)/(CLGHT*1.E-5*BF*CHAR)
C
      PSI = SLONG/RAD
      CPSI=COS(PSI)
      SPSI=SIN(PSI)
C
      P4COS=V(4)*CPSI
      P5COS=V(5)*CPSI
      P4SIN=V(4)*SPSI
      P5SIN=V(5)*SPSI
C
      VOUT(1) = V(1)+RAD*(P4SIN-P5COS+V(5))
      VOUT(2) = V(2)+RAD*(P5SIN+P4COS-V(4))
      VOUT(3) = V(3) + SLONG*V(6)
      VOUT(4)=P4COS+P5SIN
      VOUT(5)=P5COS-P4SIN
      VOUT(6) = V(6)
C
      RETURN
      END
