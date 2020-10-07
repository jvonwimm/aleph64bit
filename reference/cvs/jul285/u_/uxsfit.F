      SUBROUTINE UXSFIT(PARI,NHITS,XYZ,SIGXY,SIGZ,RSCAT,XCON,BFIEL,
     &                  NIT,CHISQ,NPAR,PARF,ERM2)
C--------------------------------------------------------------------*
C! Track helix fitter with multiple scattering                       *
C                                                                    *
C  Author: M.E.Mermikides    2 April 1987                            *
C                                                                    *
C   INPUT:                                                           *
C          PARI   = 1st estimate of track parameters                 *
C             (1) = Signed track radius in x-y plane                 *
C             (2) = Point of closest approach (d0)                   *
C             (3) = Azimuth at d0                                    *
C             (4) = Z coordinate at d0  (z0)                         *
C             (5) = Tan. of helix dip angle                          *
C             (6) = Scatt. angle                                     *
C           RSCAT = Radius of scattering layer                       *
C           NHITS = No of measured coordinates                       *
C           XYZ(NHITS) = (x,y,z) coordinates of measurements         *
C           SIGXY(NHITS) = (x,y) error of each hit                   *
C           SIGZ(NHITS) = z error of each hit                        *
C           RSCAT = Radius of equivalent m/s layer                   *
C           XCON = m/s constant = 0.015*sqrt(l), where l is          *
C                  the thickness of the scattering layer in          *
C                  radiation lengths                                 *
C           BFIEL = value of magnetic field in kG                    *
C                                                                    *
C   OUTPUT:                                                          *
C          NIT  = Number of iterations (<0 if fit failed)            *
C          CHISQ  = chisquared of fit                                *
C          NPAR = No. of parameters fitted                           *
C          PARF(NPAR) = fitted track parameters                      *
C                       (N.B. The number of degrees of freedom is    *
C                         given by 2*NHITS - NPAR)                   *
C          ERM2(NDIM) = error matrix in triangular form.             *
C                         Dimensionality NDIM = NPAR*(NPAR+1)/2      *
C                                                                    *
C                    k   do  phi0  z0  tdip theta                    *
C                   -----------------------------                    *
C                k | 1   2    4    7   11    16  |                   *
C               d0 |     3    5    8   12    17  |                   *
C             phi0 |          6    9   13    18  |                   *
C               z0 |              10   14    19  |                   *
C             tdip |                   15    20  |                   *
C            theta |                         21  |                   *
C                   -----------------------------                    *
C                                                                    *
C   Description:                                                     *
C   ===========                                                      *
C                                                                    *
C       The method is based on the formalism by D.H. Saxon           *
C   (NIM A234 (1985) 258-266), generalised to 3-dimensions for       *
C   use with the ALEPH TPC.   The procedure is to minimise the       *
C   deviations of points from the track helix. In the x-y plane      *
C   and in z, the distances are given by:                            *
C                                                                    *
C       d(xy) = 0.5*K*(r**2 - d0**2)                                 *
C               + x*sin(phi0) - y*cos(phi0) + Q*d0**2                *
C               - theta*(r - rs)   (for r > rs)                      *
C                                                                    *
C       d(z) = z - z0 - Q*tdip*delta*(1 - KQd0)/K                    *
C                                                                    *
C       delta = arcos{1-K**2*(r**2 - d0**2)/2(1- KQd0)}              *
C           r = sqrt(x**2 + y**2)                                    *
C                                                                    *
C   We form  chi**2 = Sum(d(xy)**2/err**2) + Sum(d(z)**2/erz**2)     *
C                            + (theta/theta0)**2                     *
C     (last term is operative only if there are at least 3 hits      *
C      before scattering radius, rs)                                 *
C   and derivatives with respect to the helix parameters v(i)        *
C                                                                    *
C       G(a) = d(chi**2)/dv(a)  = 2*SUM{d(i)*d'(a)/err(i)**2}        *
C                               + 2*theta/theta0**2 * d'(theta)      *
C                                                                    *
C       GG(a,b) = d2(chi**2)/dv(a)dv(b)                              *
C                                                                    *
C               =2*SUM{d(i)*d''(a,b) +d'(a)*d'(b)}                   *
C                  + 2*theta/theta0**2 * d''(theta)                  *
C                                                                    *
C      (d'(a) = d(d)/dv(a), d''(a,b) = d2(d)/dv(a)dv(b)              *
C      (d'(theta) = d(theta)/dv(a), d''(theta) = d(theta)/dv(a)dv(b) *
C                                                                    *
C   The iterative fit proceeds according to                          *
C                                                                    *
C          v(N+1) = v(N) - GG(-1)*G                                  *
C                                                                    *
C   and the error matrix at convergence is 2*GG(-1)                  *
C                                                                    *
C--------------------------------------------------------------------*
#ifndef DOC
#include "uxwork.h"
      DIMENSION PARI(6),XYZ(3,NHITS),SIGXY(NHITS),SIGZ(NHITS)
      DIMENSION PARF(6),ERM2(21)
C
      DIMENSION CUT(4)
      DOUBLE PRECISION CHISQO,CHISQN,DRES1,DRES2
      DOUBLE PRECISION STPMAX(6),HS,HS2
C Cuts and max. step sizes
      DATA NITMAX/10/
      DATA CUT/0.03, 0.01, 5.0, -0.03/
      DATA STPMAX/0.1, 0.1, 0.1, .2, 0.1, 0.1/
C Set up parameters
      CHARGE = SIGN(1.0, PARI(1))
      TRACK(1) = -CHARGE/(ABS(PARI(1)-PARI(2)))
      TRACK(2) = PARI(2)
      TRACK(3) = PARI(3)
      TRACK(4) = PARI(4)
      TRACK(5) = PARI(5)
      TRACK(6) = PARI(6)
C
C  Transfer hits to double precision work arays
C
      NSCAT = 0
      DO 100 IH = 1,NHITS
         XHIT(IH) = XYZ(1,IH)
         YHIT(IH) = XYZ(2,IH)
         ZHIT(IH) = XYZ(3,IH)
         RHIT(IH) = SQRT(XHIT(IH)**2 + YHIT(IH)**2)
         IF (RHIT(IH).LT.RSCAT) NSCAT = NSCAT + 1
         DSHIT(IH) = SIGXY(IH)
         DZHIT(IH) = SIGZ(IH)
  100 CONTINUE
C
      NPAR = 5
      IF (NSCAT.GT.3) NPAR = 6
      NDIM = NPAR*(NPAR+1)/2
      DO 5 I=1,NDIM
         ERM2(I) = -1.
    5 CONTINUE
C
C  Get number of degrees of freedom. Exit if there aren't enough points
C  (Each point has 2 contributions to chisq)
C
      CHISQO = 1.D 10
      NIT = 0
C
C  Start of iteration             <<----------------------------
C
  200 NIT = NIT + 1
C
C  Define chisq and derivative matrix
C
      CALL UXSMAT(NHITS,NPAR,RSCAT,XCON,BFIEL)
C
C  Inhibit steps in Z0, tanl (for when we are doing only circle fit)
C
C      G(4) = 0.
C      G(5) = 0.
C      DO 293 K=1,NPAR
C         DO 292 L=1,NPAR
C            IF(K.EQ.4.OR.K.EQ.5) GO TO 294
C            IF(L.NE.4.AND.L.NE.5) GO TO 292
C  294       IF(K.EQ.L) GG(K,L) = 1.
C            IF(K.NE.L) GG(K,L) = 0.
C  292    CONTINUE
C  293 CONTINUE
C
C  Invert matrix and compute correction vector for track parameters
C
      DO 21 J=1,MPAR
      DO 21 K=1,MPAR
         GGINV(K,J) = GG(K,J)
   21 CONTINUE
      CALL UMXINV(GGINV(1,1),MPAR,MPAR,NPAR,KFLG)
C
      IF (KFLG.NE.0) THEN
         NIT = -1
         GO TO 999
      ENDIF
      CHISQN = CHISQ8
      DO 410 K=1,NPAR
         XS(K) = 0.
         DO 400 L=1,NPAR
            XS(K) = XS(K) - GGINV(K,L)*G(L)
  400    CONTINUE
         CHISQN = CHISQN + 0.5*G(K)*XS(K)
  410 CONTINUE
C
C  Test for convergence and adjust step size
C
      HS = 1.0
      DRES1 = CHISQO - CHISQ8
      DRES2 = CHISQ8 - CHISQN
      IF (DRES2.LT.CUT(4)) HS = 2.0
      IF (DRES2.GT.CHISQ8) THEN
         HS2 = DRES2/CHISQ8
         IF(HS.LT.HS2) HS = HS2
      ENDIF
C
      DO 420 K=1,NPAR
         HS2 = XS(K)/STPMAX(K)
         IF(HS2.LE.0.) HS2 = - HS2
         IF(HS2.GT.HS) HS = HS2
  420 CONTINUE
      DO 430 K=1,NPAR
         XS(K) = XS(K)/HS
  430 CONTINUE
C
C  Fit has converged
C
      IF(DRES1.LT.CUT(1).AND.DRES1.GT.CUT(4)) THEN
         GO TO 500
C
C  Too many iterations
C
      ELSE IF(NIT.GE.NITMAX) THEN
         CHISQN = CHISQ8
         IF(CHISQ8.LT.CUT(3).OR.CUT(3).LE.0.0) THEN
            CHISQ8 = CHISQN
         ELSE
            NIT = - NITMAX
         ENDIF
         GO TO 500
      ELSE
C
C  Fit hasn't converged yet.  Update track parameters and go for
C  another iteration.
C
          DO 450 K=1,NPAR
             TRACK(K) = TRACK(K) + XS(K)
  450     CONTINUE
C                                     ----------------->>
          CHISQO = CHISQ8
          GO TO 200
C
      ENDIF
C
C   Save final track parameters and error matrix
C
  500 K = 0
      PARF(1) = -1./TRACK(1) +  CHARGE*TRACK(2)
      PARF(2) = TRACK(2)
      PARF(3) = TRACK(3)
      PARF(4) = TRACK(4)
      PARF(5) = TRACK(5)
      IF(NPAR.EQ.6) PARF(6) = TRACK(6)
      DO 710 I = 1,NPAR
         DO 700 J = 1,I
            K = K + 1
            ERM2(K) = 0.5*GG(I,J)
  700    CONTINUE
  710 CONTINUE
      CHISQ = CHISQ8
C
  999 RETURN
      END
#endif
