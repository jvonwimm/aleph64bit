C
      SUBROUTINE HYTRSF (X,Y,IP,IM)
C --------------------------------------------
CKEY HCALDES HCAL ENDCAP TRANSFORM COORDINATES / USER
C
C! Trasform to plane coordinate system for the end-caps
C!
C!    Author      : G.Zito   85/05/21
C!    modified by : G.Catanesi 87/10/21
C!
C!   -Called by : HCCOIN
C!   -Calls     : none
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
      PARAMETER( LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER( LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER( LPHCT = 4)
      PARAMETER( LPHCBM = 24,LPHCES = 6)
C ------------------------------------------------------------
C
      IF (IP.EQ.LPECA) THEN
         IF (IM.EQ. 3.OR. IM.EQ.6)                               GOTO 10
      ELSE
         IF (IM.EQ. 1.OR. IM.EQ.4)                               GOTO 10
      ENDIF
                                                                 GOTO 20
C
   10 XI = X
      YI = Y
      R =SQRT(XI*XI+YI*YI)
      XR = XI/R
      IF (ABS(XR).GE.1.) XR =SIGN (1.,XI)
      ANG = PIBY6-ACOS(XR)
      IF(ANG.GT.0.) ANG = 2.*ANG
      IF(ANG.LT.0.)ANG = TWOPI+2.*ANG
      X= COS(ANG)*XI - SIN(ANG)*YI
      Y= SIN(ANG)*XI + COS(ANG)*YI
C
   20 XI = Y
      Y = X
      X = XI
C
      END
