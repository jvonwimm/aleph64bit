      PARAMETER (MPAR=6,MHIT=35)
      COMMON /UXWORK/  XHIT(MHIT),YHIT(MHIT),ZHIT(MHIT),RHIT(MHIT),
     1                 DSHIT(MHIT),DZHIT(MHIT),RESID(2,MHIT),
     2                 G(MPAR),GG(MPAR,MPAR),GGINV(MPAR,MPAR),XS(MPAR),
     3                 TRACK(MPAR),CHARGE,DDDV(MPAR),DZDV(MPAR),
     4                 D2DDV2(MPAR,MPAR),D2ZDV2(MPAR,MPAR),CHISQ8
      DOUBLE PRECISION XHIT,YHIT,ZHIT,RHIT,DSHIT,DZHIT,
     1                 G,GG,GGINV,XS,TRACK,CHARGE,DDDV,DZDV,
     2                 D2DDV2,D2ZDV2,RESID,CHISQ8
C
#if defined(DOC)
C! Common for communication between fit routines ------------
C
C   XHIT,YHIT,ZHIT = Measurement coordinates
C   WSHIT,WZHIT   = Weights for (x,y) and z measuremets
C
C   G(MPAR) = 1st derivatives of chisq. with respect to helix
C             parameters {k,d0,phi0,z0,tdip, (theta)}
C   G(MPAR,MPAR) = Matrix of second derivatives
C   GGINV(MPAR,MPAR) = Inverse matrix of second derivatives
C   DDDV(MPAR)  = 1st derivatives of (xy) deviations of hits from helix
C                 with respect to helix parameters d(dxy)/dv(i)
C   DZDV(MPAR)  = 1st derivatives of (z) deviations of hits from helix
C                 with respect to helix parameters d(dz)/dv(i)
C   D2DDV2(MPAR,MPAR) = Matrix of second derivatives d2(dxy)/dv(i),dv(j)
C   D2ZDV2(MPAR,MPAR) = Matrix of second derivatives d2(dz)/dv(i),dv(j)
C
#endif
