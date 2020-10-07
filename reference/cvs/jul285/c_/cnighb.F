      SUBROUTINE CNIGHB(XPT,RMS,JT,IFI,KS,ITR,IECOB,IFND)
C***********************************************************************
C! Loop on neighbours of point xpt in hstorey                          *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  871023                                    *
C!  MODIFIED : A. BONISSENT  901206                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : HSTO,HSDA                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   xpt(3)   = coordinates point (X,Y,Z)                              *
C!   rms      = multiple scattering                                    *
C!   jt       = theta                                                  *
C!   ifi      = phi                                                    *
C!   ks       = stack                                                  *
C!                                                                     *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "alcons.h"
#include "cthres.h"
#include "hcnamc.h"
#include "hstojj.h"
#include "hsdajj.h"
#include "hclujj.h"
#include "bcs.h"
      DIMENSION XPT(*),CORNR(3,8),XCENT(3)
#include "bmacro.h"
      KHCLU=IW(NAHCLU)
C
C   Temporary fixup until number of corners is restored !!!!!
C
      LHPOI=8
      CALL HSRCR('ALEPH',JT,IFI,KS,LHPOI,CORNR)
C
C?  Make coordinates of storey center
      IF(LHPOI.NE.8)GO TO 998
C
      DO 10 IC=1,3
      XCENT(IC)=0.
      DO 15 ICORN=1,8
   15 XCENT(IC)=XCENT(IC)+CORNR(IC,ICORN)
      XCENT(IC)=XCENT(IC)/8.
   10 CONTINUE
      KHSTO=IW(NAHSTO)
      KHSDA=IW(NAHSDA)
      NHSTO=LROWS(KHSTO)
      DO 20 IST=1,NHSTO
C
C?  eliminate storeys which are in a different stack
C
      KLOC=ITABL(KHSDA,IST,JHSDSN)
      IHCLU=ITABL(KHSDA,IST,JHSDCN)
C
C? Skip noisy hclusters
C
C
C  Noisy clusters should not be skipped !
C  Ignore NOISE
C      NOISE=ITABL(KHCLU,IHCLU,JHCLNF)
C      IF(KLOC.EQ.KS.AND.IHCLU.GT.0.AND.NOISE.EQ.0)THEN
      IF(KLOC.EQ.KS.AND.IHCLU.GT.0)THEN
C
C?  eliminate the storey in which we are
C
         ILOC=ITABL(KHSDA,IST,JHSDPI)
         JLOC=ITABL(KHSDA,IST,JHSDTI)
         IF(ILOC.NE.IFI.OR.JLOC.NE.JT)THEN
            DIST=SQRT((RTABL(KHSTO,IST,JHSTSX)-XPT(1))**2+
     +          (RTABL(KHSTO,IST,JHSTSY)-XPT(2))**2+
     +          (RTABL(KHSTO,IST,JHSTSZ)-XPT(3))**2)
C
C?  eliminate storeys which are more than 50 cm. away
C
            IF(DIST.LT.50.)THEN
               CALL HUSRAN(JLOC,ILOC,KLOC,THC,PHC,DTH,DPH)
C
C?   Compute theta distance
C
               RX=SQRT(XPT(1)**2+XPT(2)**2+XPT(3)**2)
               THETX=ACOS(XPT(3)/RX)
               PHIX=ATAN2(XPT(2),XPT(1))
               IF(PHIX.LT.0.)PHIX=PHIX+TWOPI
               THETI=THC-DTH*0.5-THETX
               THETS=THC+DTH*0.5-THETX
               IF(THETI*THETS.LT.0.)THEN
                  THETA=0.
               ELSE
                  THETA=AMIN1(ABS(THETI),ABS(THETS))
               ENDIF
C
C?   transform dist from rads into cm
C
               DTHET=THETA*RX
C
C?  compute phi distance
C
               PHII=PHC-DPH*0.5-PHIX
               IF(ABS(PHII).GT.PI)PHII=PHII-SIGN(TWOPI,PHII)
               PHIS=PHC+DPH*0.5-PHIX
               IF(ABS(PHIS).GT.PI)PHIS=PHIS-SIGN(TWOPI,PHIS)
               IF(PHII*PHIS.LT.0.)THEN
                  DPHI=0.
               ELSE
C
C?   transform dist from rads into cm
C
                  DPHI=AMIN1(ABS(PHII),ABS(PHIS))*RX*SIN(THETX)
               ENDIF
C
C?   and divide by rms
C
               DLOC=SQRT(DTHET**2+DPHI**2)/RMS
               IF(DLOC.LT.PNMICT)THEN
                  CALL CINSRL(ITR,IHCLU,DLOC,IECOB)
                  IFND=1
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   20 CONTINUE
  999 CONTINUE
      RETURN
  998 CALL RERROR(' CNIGHB',1,' MORE THAN 8 CORNERS AT STOREY, ABORT')
      RETURN
      END
#endif
