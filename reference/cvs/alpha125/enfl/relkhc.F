      SUBROUTINE RELKHC(IHCOBJ,ICHARG,ANGL,DELTAE)
C--------------------------------------------------------------------
C!  Force a link of an isolated HCAL cluster to a TPC track
C   when JULIA failed...
C
C   Input :   -- ihcobj,  ALPHA index of the HCAL cluster
C   Output:   -- icharg,  ALPHA index of the TPC track
C             -- angl  ,  angle between HCAL and extrapolation (deg.)
C             -- deltae,  energy difference (GeV)
C
C  Patrick Janot -- 05 Sep 1990
C--------------------------------------------------------------------
#ifndef DOC
#include "intval.h"
      COMMON / hlkbk / kcha, nchj(200)
#include "cuphco.h"
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qhac.h"
#include "qmacro.h"
C--------------------------------------------------------------------
C  Find the track closest to the HCAL cluster ihcobj
C
      icharg = 0
      anglmin = 999999.
      angl = anglmin
      icluhc = ktn(ihcobj)
      pzh2 = COS(twh(1,icluhc))
      pyh2 = SIN(twh(1,icluhc)) * SIN(pwh(1,icluhc))
      pxh2 = SIN(twh(1,icluhc)) * COS(pwh(1,icluhc))
C
      DO 1 jcha = 1 , kchatt
        nchtr = ncht(jcha)
        DO lcha = 1 , kcha
          IF ( nchj(lcha) .EQ. nchtr ) GOTO 1
        ENDDO
        pxh = pexhca(jcha,1)
        pyh = pexhca(jcha,2)
        pzh = pexhca(jcha,3)
        dis = SQRT(pxh**2+pyh**2+pzh**2)
        angh = (pxh * qx(ihcobj)
     .       +  pyh * qy(ihcobj)
     .       +  pzh * qz(ihcobj))
     .       / dis
     .       / SQRT(qx(ihcobj)**2+qy(ihcobj)**2+qz(ihcobj)**2)
        IF ( angh .GE. 1. ) angh = .99999999
        IF ( angh .LE.-1. ) angh =-.99999999
        angh = acos(angh) * 180. / qqpi
C
        IF ( etw(1,icluhc) .GT. 0. ) THEN
          ang2 = (pxh * pxh2 + pyh * pyh2 + pzh * pzh2) / dis
          IF ( ang2 .GE. 1. ) ang2 = .99999999
          IF ( ang2 .LE.-1. ) ang2 =-.99999999
          ang2 = acos(ang2) * 180. / qqpi
          angh = AMIN1(angh,ang2)
        ENDIF
C
        IF ( angh .LT. anglmin ) THEN
          anglmin = angh
          angl = angh
          icharg = ncht(jcha)
          deltae = delt(jcha) - qe(ihcobj)
        ENDIF
    1 CONTINUE
C
  999 RETURN
      END
#endif
