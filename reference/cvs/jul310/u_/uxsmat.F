      SUBROUTINE UXSMAT(NHITS,NPAR,RSCAT,ACON,BFIEL)
C--------------------------------------------------------------------*
C!  Auxiliary routine to UXSFIT.                                     *
C  Computes chisq. and derivatives                                   *
C  Author:   M.E.Mermikides  2 April 1987                            *
C  Modified: D.Smith         991027  Double precision fixes for Linux
C--------------------------------------------------------------------*
#ifndef DOC
#include "uxwork.h"
      DOUBLE PRECISION SINPHI,COSPHI,OMDKQ,SOMDKQ,
     1                 ERR2,ERZ2,DXY,DZ,R2MD2,OTHE2,RHO,
     2                 CDEL,DELTA,T1,DTDK,DTDD,DIR
C
C  For converting  rad(cm), B(kG) -> Momentum(GeV/c)
      DATA CLIGHT /0.0002997924/
C
      CHISQ8 = 0D0
      DO 20 I=1,NPAR
         G(I) = 0D0
         DO 30 J=1,NPAR
            GG(J,I) = 0D0
   30    CONTINUE
   20 CONTINUE
C
      SINPHI = SIN(TRACK(3))
      COSPHI = COS(TRACK(3))
      OMDKQ = 1D0 - TRACK(1)*TRACK(2)*CHARGE
      SOMDKQ = SQRT(ABS(OMDKQ))
C  Multiple scattering terms
      IF (NPAR.EQ.6) THEN
         RHO = TRACK(2) - CHARGE/TRACK(1)
         OTHE2 = (CLIGHT*BFIEL*RHO/ACON)**2*(1D0+TRACK(5))**2
      ENDIF
C
      DO 100 IH = 1,NHITS
C
C  DXY = deviation in xy, DZ = deviation in z
C
         R2MD2 = RHIT(IH)**2 - TRACK(2)**2
         IF (R2MD2.LT.0D0) THEN
            R2MD2 = -R2MD2
            DIR = -1D0
         ELSE
            DIR = 1D0
         ENDIF
         DXY=0.5D0*R2MD2*TRACK(1) + XHIT(IH)*SINPHI - YHIT(IH)*COSPHI
     &                            + CHARGE*TRACK(2)
         IF(RHIT(IH).GT.RSCAT.AND.NPAR.EQ.6)
     &            DXY = DXY - (RHIT(IH)-RSCAT)*TRACK(6)
         ERR2 = DSHIT(IH)**2
         ERZ2 = DZHIT(IH)**2
         RESID(1,IH) = DXY/DSHIT(IH)
         CDEL = 1D0 - 0.5D0*TRACK(1)**2*R2MD2/OMDKQ
         IF (CDEL.GT.1D0) THEN
            CDEL = 1D0
         ELSEIF (CDEL.LT.-1D0) THEN
            CDEL = -1D0
         ENDIF
         DELTA = ACOS(CDEL)
         DZ = ZHIT(IH) -
     &         (TRACK(4) - TRACK(5)*DIR*DELTA*CHARGE*OMDKQ/TRACK(1))
         RESID(2,IH) = DZ/DZHIT(IH)
         CHISQ8 = CHISQ8 + RESID(1,IH)**2 + RESID(2,IH)**2
C  DDDV(i) 1st derivatives of DXY w.r.t. helix parameters
         DDDV(1) = 0.5D0*R2MD2
         DDDV(2) = -TRACK(1)*TRACK(2) + CHARGE
         DDDV(3) = XHIT(IH)*COSPHI + YHIT(IH)*SINPHI
         DDDV(4) = 0D0
         DDDV(5) = 0D0
         DDDV(6) = 0D0
         IF(NPAR.EQ.6.AND.RHIT(IH).GT.RSCAT) DDDV(6) = RSCAT-RHIT(IH)
C  and derivatives of DZ
         T1 = SQRT(R2MD2)/SOMDKQ
         DZDV(1) = TRACK(5)*TRACK(2)*CHARGE*T1/2D0
         DZDV(2) = TRACK(5)*TRACK(1)*CHARGE*T1/2D0
         DZDV(3) = 0D0
         DZDV(4) = -1D0
         DZDV(5) = -OMDKQ*T1
         DZDV(6) = 0D0
C
C  2nd derivatives
C
         DO 40 I=1,NPAR
            DO 50 J=1,NPAR
               D2DDV2(J,I) = 0D0
               D2ZDV2(J,I) = 0D0
   50       CONTINUE
   40    CONTINUE
         D2DDV2(1,2) = -TRACK(2)
         D2DDV2(2,1) = -TRACK(2)
         D2DDV2(2,2) = -TRACK(1)
         D2DDV2(3,3) = -XHIT(IH)*SINPHI + YHIT(IH)*COSPHI
C
         DTDK =  CHARGE*TRACK(2)*T1/(2D0*OMDKQ)
         IF (R2MD2.EQ.0D0) THEN
            DTDD = 0D0
         ELSE
            DTDD = -TRACK(2)*T1/R2MD2 +T1*TRACK(1)*CHARGE/(2D0*OMDKQ)
         ENDIF
         D2ZDV2(1,1) = TRACK(5)*TRACK(2)*CHARGE*DTDK/2D0
         D2ZDV2(1,2) = TRACK(5)*TRACK(2)*CHARGE*DTDD/2D0 +
     &                 T1*CHARGE*TRACK(5)/2D0
         D2ZDV2(1,5) = TRACK(2)*CHARGE*T1/2D0
         D2ZDV2(2,2) = TRACK(5)*TRACK(1)*CHARGE*DTDD/2D0
         D2ZDV2(2,5) = TRACK(1)*CHARGE*T1/2D0
         D2ZDV2(2,1) = D2ZDV2(1,2)
         D2ZDV2(5,1) = D2ZDV2(1,5)
         D2ZDV2(5,2) = D2ZDV2(2,5)
C
         DO 270 K = 1,NPAR
            G(K) = G(K) + 2D0*DXY*DDDV(K)/ERR2+2D0*DZ*DZDV(K)/ERZ2
            DO 280 L = 1,K
               GG(K,L) = GG(K,L) +
     &          2D0*(DXY*D2DDV2(K,L)+DDDV(K)*DDDV(L))/ERR2 +
     &          2D0*( DZ*D2ZDV2(K,L)+DZDV(K)*DZDV(L))/ERZ2
                IF(L.LT.K) GG(L,K) = GG(K,L)
  280       CONTINUE
  270     CONTINUE
C
  100 CONTINUE
C
C Add m/s term to chisq and derivatives
C
      IF (NPAR.EQ.6) THEN
         CHISQ8 = CHISQ8 + OTHE2*TRACK(6)**2
         G(6) =  G(6) + 2D0*OTHE2*TRACK(6)
         GG(6,6) = GG(6,6) + 2D0*OTHE2
      ENDIF
C
      RETURN
      END
#endif
