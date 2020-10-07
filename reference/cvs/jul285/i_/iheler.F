      SUBROUTINE IHELER (RINV,TL,P0,D0,Z0,CM,
     +                   R,PHI,Z,ZERR,PHERR,IFLAG)
C
C------------------------------------------------------------------------------
C!    Calculate errors in coordinates on a helix.
C!
C!    Author                M J Phillips with J Sedgbeer    01-Jan-1991
C!
C!    Input             (  RINV       inverse radius of curvature
C!                      (  TL         tan (lambda) = tan (dip angle)
C!         usual        (  P0         phi-zero
C!          track       (  D0         d-zero
C!           parameters (  Z0         z-zero
C!                         CM(15)     Covariance matrix
C!
C!                                           RINV  TL  P0  D0  Z0
C!                                          ----------------------
C!                                    RINV     1    2   4   7  11
C!                                    TL            3   5   8  12
C!                                    P0                6   9  13
C!                                    D0                   10  14
C!                                    Z0                       15
C!
C!                         R          ) Coordinates of point on track.
C!                         PHI        ) NB There is no check that this
C!                         Z          ) point actually lies on the
C!                                      specified helix
C!
C!    Output               ZERR       Error in z-coordinate
C!                         PHERR      Error in phi-coordinate
C!                         IFLAG = 0  Successful
C!                                 1  Unsuccessful
C!
C!    Description
C!    ===========
C!    The errors of a point on a helix are calculated, as a function
C!    of the coordinates of that point, the parameters of the track,
C!    and the errors in those parameters.
C!
C?
C!======================================================================
#ifndef DOC
C
C .............................................................................
C
      REAL CM(15)
C
      IFLAG = 0
C
      W = RINV
C
      VARWW = CM (1)
      VARTT = CM (3)
      VARPP = CM (6)
      VARDD = CM (10)
      VARZZ = CM (15)
C
      COVWT = CM (2)
      COVWP = CM (4)
      COVWD = CM (7)
      COVWZ = CM (11)
      COVTP = CM (5)
      COVTD = CM (8)
      COVTZ = CM (12)
      COVPD = CM (9)
      COVPZ = CM (13)
      COVDZ = CM (14)
C
      XX = (R**2.-D0**2)/(1.-D0*W)
          IF (XX.LT.0.0) GOTO 120
      X = W/2.*SQRT(XX)
          IF (ABS(X).GE.1.) GOTO 120
C
      DEZW = (Z0-Z)/W- TL/(W**2)*X/SQRT(1.-X**2)*(2.+D0*W/(1.-D0*W))
      DEZT = (Z-Z0)/TL
      DEZD = -2.*TL/W*X/SQRT(1.-X**2)*(W/(1.-D0*W)/2.-D0/(R**2-D0**2))
C
      ZERR = DEZW * (      DEZW * VARWW +
     +                2. * DEZT * COVWT +
     +                2. * DEZD * COVWD +
     +                2. *        COVWZ   ) +
     +       DEZT * (      DEZT * VARTT +
     +                2. * DEZD * COVTD +
     +                2. *        COVTZ   ) +
     +       DEZD * (      DEZD * VARDD +
     +                2. *        COVDZ   ) +
     +       VARZZ
C
C
      Y = ABS(W)/R/2.*(R**2-D0**2)/(D0*W-1.)+ABS(W)/W*D0/R
          IF (ABS(Y).GE.1.) GOTO 120
C
      DEPW = (R**2-D0**2)/2./R/(D0*W-1.)**2/SQRT(1.-Y**2)
      DEPD = -0.5/R/SQRT(1.-Y**2)*(1.+(1.-(W*R)**2)/(D0*W-1.)**2)
C
      PHERR = DEPW * (      DEPW * VARWW +
     +                 2. *        COVWP +
     +                 2. * DEPD * COVWD ) +
     +        VARPP                       +
     +        DEPD * (      DEPD * VARDD +
     +                 2. *        COVPD )
C
C   Rounding errors ?
C
      IF (ZERR.LT.0.0) ZERR=0.0
      IF (PHERR.LT.0.0) PHERR=0.0
C
      ZERR  = SQRT (ZERR)
      PHERR = SQRT (PHERR)
C
      GOTO 130
 120  IFLAG = 1
 130  RETURN
      END
#endif
