      SUBROUTINE UVPMAT(NTK,NPAR)
C--------------------------------------------------------------------*
C! Auxiliary routine to UVPFIT to compute chisq and derivative       *
C  matrices                                                          *
C  Author:   M.E.Mermikides  27 May 1987                             *
C  Modified: D.Smith         991027  Double precision fixes for Linux
C--------------------------------------------------------------------*
#ifndef DOC
#include "uvpwrk.h"
C
      DOUBLE PRECISION DDDV(3),DZDV(3),D2DDV2(3,3),D2ZDV2(3,3)
      DOUBLE PRECISION DELTA,S,SS,SSS,T,TT,DXY,DZ,
     &                 XTA,YTA,D2A,XTB,YTB,D2B,XT,YT,U0,U1,V1,
     &                 SIGX2,SIGZ2,RV2,DIR,DX,DY, DXTDA,DYTDA,
     &                 DXTDB,DYTDB,DTDA,DTDB
C
      PARAMETER (PI=3.141592653589, PIBY2=PI/2.)
C
      CHISQ8 = 0D0
C
      DO 10 I=1,3
        G(I)=0D0
        DO 12 J=1,3
           GG(J,I) = 0D0
   12   CONTINUE
   10 CONTINUE
C
      RV2 = XV(1)*XV(1) + XV(2)*XV(2)
      DO 100 I = 1,NTK
         DX = XV(1) - XYI(1,I)
         DY = XV(2) - XYI(2,I)
         SS = DX*DX + DY*DY
         S = DSQRT(SS)
         SSS = SS*S
C Deviation in (x,y)
         DXY = S - RADI(I)
C Derivatives of d(xy)
         DDDV(1) = DX/S
         DDDV(2) = DY/S
         DDDV(3) = 0D0
         DO 21 J=1,3
         DO 21 K=1,3
            D2DDV2(K,J) = 0D0
   21    CONTINUE
         D2DDV2(1,1) = 1D0/S - DX*DX/SSS
         D2DDV2(1,2) = - DX*DY/SSS
         D2DDV2(2,2) = 1D0/S - DY*DY/SSS
         D2DDV2(2,1) = D2DDV2(1,2)
C 2D fit
         IF (NPAR.EQ.2) THEN
            DZ = 0D0
            DO 31 J=1,3
               DZDV(J)=0D0
               DO 32 K=1,3
                  D2ZDV2(K,J) = 0D0
   32          CONTINUE
   31       CONTINUE
C 3D fit
         ELSE
C Compute deviations in Z and derivatives.  First we find the
C coordinates at the point on the track closest to (A,B)
            XTA = XYI(1,I) + DX*RADI(I)/S
            YTA = XYI(2,I) + DY*RADI(I)/S
            D2A = ABS(XTA*XTA + YTA*YTA - RV2)
            XTB = XYI(1,I) - DX*RADI(I)/S
            YTB = XYI(2,I) - DY*RADI(I)/S
            D2B = ABS(XTB*XTB + YTB*YTB - RV2)
            IF (D2A.LT.D2B) THEN
               SOL = +1.
               XT = XTA
               YT = YTA
            ELSE
               SOL = -1.
               XT = XTB
               YT = YTB
            ENDIF
C Find z at (xt,yt); we have to take care of sign of turning
C angle from start of track to this point.
            U0 = XYI(1,I)*CPHC(I)+ XYI(2,I)*SPHC(I) - RADI(I)
            U1 = XT*CPHC(I) + YT*SPHC(I)
            V1 = YT*CPHC(I) - XT*SPHC(I)
            DIR = DSIGN(1.D0,Q(I)*V1)
            T = DSQRT((XT-X0(I))**2 + (YT-Y0(I))**2)
            DELTA = 2D0*DIR*DASIN( DMIN1( T/(2D0*RADI(I)),1.D0 ) )
C
            DZ = XV(3) - (Z0(I) + RADI(I)*DELTA*TDIP(I))
C
C Derivatives of d(z), calculated in approximation of arc length by
C chord:
C DZ = XV(3) - Z0 - DIR*TDIP(I)*SQRT((XT-X0)**2 + (YT-Y0)**2)
C
            DXTDA = SOL*RADI(I)*(1D0 - DX*DX/SS) /S
            DYTDA =-SOL*RADI(I)*DX*DY/SSS
            DXTDB = DYTDA
            DYTDB = SOL*RADI(I)*(1D0 - DY*DY/SS) /S
            DTDA = ((XT-X0(I))*DXTDA + (YT-Y0(I))*DYTDA)/T
            DTDB = ((XT-X0(I))*DXTDB + (YT-Y0(I))*DYTDB)/T
            DZDV(1) = -DIR*TDIP(I)*DTDA
            DZDV(2) = -DIR*TDIP(I)*DTDB
            DZDV(3) = 1D0
            DO 41 J=1,3
               DO 41 K=1,3
                  D2ZDV2(K,J) = 0D0
   41       CONTINUE
C Second derivatives of dz are set to zero
         ENDIF
C
C  Sum terms of chisq.
C
         SIGX2 = SIGX(I)**2
         SIGZ2 = SIGZ(I)**2
         CHISQ8 = CHISQ8 + DXY*DXY/SIGX2 + DZ*DZ/SIGZ2
C  Derivatives of chisq.
         DO 320 K = 1,NPAR
            G(K) = G(K)+2D0*DXY*DDDV(K)/SIGX2+2D0*DZ*DZDV(K)/SIGZ2
            DO 325 L = 1,K
               GG(K,L) = GG(K,L) +
     &          2D0*(DXY*D2DDV2(K,L)+DDDV(K)*DDDV(L))/SIGX2 +
     &          2D0*( DZ*D2ZDV2(K,L)+DZDV(K)*DZDV(L))/SIGZ2
                IF (L.LT.K) GG(L,K) = GG(K,L)
  325       CONTINUE
  320    CONTINUE
C
  100 CONTINUE
      RETURN
      END
#endif
