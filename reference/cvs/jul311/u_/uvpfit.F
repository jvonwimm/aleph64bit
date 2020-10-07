      SUBROUTINE UVPFIT(NTRA,TRPAR,ERRX,ERRZ,NPAR,
     &                         NIT,NDF,CHISQ,VXP,ERRVX)
C--------------------------------------------------------------------*
C! 3-D vertex fitter                                                 *
C  Author:   M.E.Mermikides  27 May 1987                             *
C  Modified: D.Smith         991027  Double precision fixes for Linux
C                                                                    *
C   INPUT:                                                           *
C           NTRA = Number of tracks to be fitted                     *
C           TRPAR(6*NTRA) = Array of parameters of each track        *
C                            (rho,d0,phi0,zo,tanl,Theta)             *
C           ERRX(NTRA) = Errors on track deviation from vertex in    *
C                        (x,y) plane. (For tracks coming from near   *
C                        the origin this corresponds closely to the  *
C                        error on d0)                                *
C           ERRZ(NTRA) = Errors on track deviation from vertex in    *
C                        z plane. (For tracks coming from near       *
C                        the origin this corresponds closely to the  *
C                        error on z0)                                *
C           NPAR = No of parameters to be fitted (2 -> xy, 3 -> xyz) *
C                                                                    *
C   OUTPUT:                                                          *
C          NIT  = Number of iterations (<0 if fit failed)            *
C          CHISQ  = chisquared of fit                                *
C          NDF = number of degrees of freedom                        *
C          VXP(NPAR) = fitted vertex coordinates {x,y(,z)}           *
C          ERRVX(NDIM) = Error matrix (=Inverse covariance matrix)   *
C                 of fitted vertex position in triangular form:      *
C                {xx,xy,yy(,xz,yz.zz)};    (NDIM = NPAR*(NPAR+1)/2)  *
C                                                                    *
C      CHISQ IS CALCULATED, 1ST DERIVATIVES IN G(3), 2ND IN GG(3,3)  *
C                                                                    *
C   Description:                                                     *
C   ===========                                                      *
C                                                                    *
C       The method is based on the formalism by D.H. Saxon           *
C   (NIM A234 (1985) 258-266), generalised to 3-dimensions for       *
C   use with the ALEPH tracking detectors.   The procedure is to     *
C   minimise the deviations of the tracks from the common vertex     *
C   at the coordinates (A,B,C).                                      *
C                                                                    *
C       In the x-y plane and in z, the deviations of a track         *
C   from the vertex point (A,B,C) are given by:                      *
C                                                                    *
C       d(xy) = s - rho                                              *
C                                                                    *
C       d(z) = z - C - rho*tdip*delta                                *
C                                                                    *
C       where                                                        *
C          s =sqrt[(A-xc)**2 - (B-yc)**2]                            *
C          rho = radius of curvature of track circle                 *
C          tdip = tangent of track helix dip angle                   *
C          delta = 2*arcsin{sqrt[(xt-x0)**2 + (yt-y0)**2]/(2*rho)}   *
C          xt = xc +/- rho*(A-xc)/s                                  *
C          yt = yc +/- rho*(B-xc)/s                                  *
C          x0,y0 = coordinates at point of closest approach of track *
C                  to origin (d0)                                    *
C                                                                    *
C    Fitting is done using a matrix inversion method on the vector   *
C    (A,B,C)                                                         *
C                                                                    *
C   Chisq = d(xy)**2/SIGX**2 + d(z)**2/SIGZ**2                       *
C                                                                    *
C--------------------------------------------------------------------*
#ifndef DOC
C
#include "uvpwrk.h"
      DIMENSION TRPAR(6*NTRA),ERRX(NTRA),ERRZ(NTRA),VXP(3),ERRVX(6)
C
      DOUBLE PRECISION DRES1,DRES2,CUT(4)
      DOUBLE PRECISION D0,PHI0,PHIC, STPMAX,STEP,ST
C
C
      PARAMETER (PI=3.141592653589, PIBY2=PI/2.)
C
      DATA CUT/0.03D0,0.01D0,0D0,-0.03D0/
      DATA NITMAX/10/
      DATA STPMAX/2D0/
C
      VXP(1) = 0.0001
      VXP(2) = 0.0001
      VXP(3) = 0.0001
      NDIM = NPAR*(NPAR+1)/2
      DO 50 I=1,NDIM
         ERRVX(I) = 0.
   50 CONTINUE
C
C Starting values for vertex position (A,B,C)
C
      XV(1) = 0.0001D0
      XV(2) = 0.0001D0
      XV(3) = 0.0001D0
      NTK = MIN(NTRA,MXTRA)
C No of degrees of freedom
      NDF = (NPAR-1)*NTK - NPAR
C
C Loop over tracks to get centre, radius and errors
C
      DO 100 I = 1,NTK
         IND = (I-1)*6
         D0 = TRPAR(IND+2)
         PHI0 = TRPAR(IND+3)
         Q(I) = SIGN(1.,TRPAR(IND+1))
         RADI(I) = ABS(TRPAR(IND+1))
         PHIC = PHI0 - Q(I)*DBLE(PIBY2)
         CPHC(I) = DCOS(PHIC)
         SPHC(I) = DSIN(PHIC)
C Coordinates of circle centre (xc(i),yc(i))
         XYI(1,I) = (RADI(I) - D0)*CPHC(I)
         XYI(2,I) = (RADI(I) - D0)*SPHC(I)
C Coordinates at D0
         X0(I) = - D0*CPHC(I)
         Y0(I) = - D0*SPHC(I)
         Z0(I) = TRPAR(IND+4)
         TDIP(I) = TRPAR(IND + 5)
C Errors
         SIGX(I) = ERRX(I)
         SIGZ(I) = ERRZ(I)
  100 CONTINUE
C
      CHISQO = 1.D10
      NIT = 0
C
C  Start of iteration       <-------------------------------------
  200 NIT = NIT + 1
C
C Compute derivatives and matrices
C
      CALL UVPMAT(NTRA,NPAR)
C
      DO 11 J=1,3
      DO 11 K=1,3
         GGINV(K,J)= GG(K,J)
   11 CONTINUE
      CALL UMXINV(GGINV(1,1),3,3,NPAR,KFLG)
      IF (KFLG.NE.0) THEN
         NIT = -1
         GO TO 999
      ENDIF
C
C  CHISQO = Chisq of previous iteration
C  CHISQ8 = Current value of chi squared
C  CHISQN = Updated chisq using current corrections
C
      CHISQN = CHISQ8
      DO 410 K=1,NPAR
         XS(K) = 0D0
         DO 400 L=1,NPAR
            XS(K) = XS(K) - GGINV(K,L)*G(L)
  400    CONTINUE
         CHISQN = CHISQN + 0.5D0*G(K)*XS(K)
  410 CONTINUE
C
C  Test for convergence and adjust step size
C
      DRES1 = CHISQO - CHISQ8
      DRES2 = CHISQ8 - CHISQN
      STEP = 1D0
      IF(DRES2.LT.CUT(4)) STEP=0.5D0
C Step cut
      IF(DRES2.GE.CHISQ8) STEP = CHISQ8/DRES2
      DO 414 K = 1,NPAR
         ST = XS(K)*STEP
         IF(K.EQ.3) ST = ST * 0.25D0
         IF(ST.LE.0D0) ST = -ST
         IF(ST.GE.STPMAX) STEP = STEP*STPMAX/ST
  414 CONTINUE
      IF(STEP.NE.1D0) THEN
         DO 412 K = 1,3
            XS(K) = XS(K)*STEP
  412    CONTINUE
      ENDIF
C
C Convergence test 1:  Change in chisq within given limits
C
      IF(DRES1.LT.CUT(1).AND.DRES1.GT.CUT(4)) GOTO 500
C
C Update parameter vector
C
      DO 450 K = 1,NPAR
         XV(K) = XV(K) + XS(K)
  450 CONTINUE
C
C Convergence test 2:  Chisq correction for full step below given limit
C
      IF(DRES2.LT.CUT(2).AND.STEP.EQ.1D0) THEN
         CHISQ8 = CHISQN
         GO TO 500
      ENDIF
C
      CHISQO = CHISQ8
C
C Do another iteration   ------------------------------->
C
      IF(NIT.LT.NITMAX) GOTO 200
C
C Stop after NITMAX iterations, keep best chisq
C
      IF(CHISQ8.LT.CUT(3).OR.CUT(3).LE.0D0) CHISQ8 = CHISQN
C
C   Save final parameters and error matrix
C
  500 VXP(1) = XV(1)
      VXP(2) = XV(2)
      VXP(3) = XV(3)
      CHISQ = CHISQ8
      ERRVX(1) = 0.5D0*GG(1,1)
      ERRVX(2) = 0.5D0*GG(1,2)
      ERRVX(3) = 0.5D0*GG(2,2)
      IF (NPAR.EQ.3) THEN
         ERRVX(4) = 0.5D0*GG(3,1)
         ERRVX(5) = 0.5D0*GG(3,2)
         ERRVX(6) = 0.5D0*GG(3,3)
      ENDIF
C
  999 RETURN
      END
#endif
