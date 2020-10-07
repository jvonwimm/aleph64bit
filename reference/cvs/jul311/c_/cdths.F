      SUBROUTINE CDTHS(TXX,TRTF,IKSCK,NPTS,ISTH,DIST)
C***********************************************************************
C! Compute track-hstorey distance                                      *
C!  for one mip track                                                  *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  871023                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : HSTO                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   Txx   = coordinates along extrapolated track (X,Y,Z)              *
C!   Trtf  = coordinates along extrapolated track (R,T,FI,Rms)         *
C!   Isth  = hcal storey number                                        *
C!   dist  = distance/sigma multiple scattering                        *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "hstojj.h"
#include "hsdajj.h"
#include "hcnamc.h"
#include "bcs.h"
      DIMENSION XCENT(3),PLANS(2,2)
      DIMENSION TXX(3,*),TRTF(4,*),IKSCK(6,*)
C
C   correspondence between faces and corners :
C   first index is the face number,
C   1,3 : parallel to theta axis
C   2,4 : parallel to phi axis
C   second is the corner number in the face.
C   1,2 : inner line
C   3,4 : outer line
C
      DIMENSION IFACE(4,4)
      DATA IFACE/1,2,5,6,
     +           2,4,6,8,
     +           3,4,7,8,
     +           1,3,5,7/
      DATA DISLM/100./
#include "bmacro.h"
      KHSTO=IW(NAHSTO)
C
C?  get x,y,z coordinates of storey center
C
      XCENT(1)=RTABL(KHSTO,ISTH,JHSTSX)
      XCENT(2)=RTABL(KHSTO,ISTH,JHSTSY)
      XCENT(3)=RTABL(KHSTO,ISTH,JHSTSZ)
C
C?   set distance to a large value
C
      DIST=10000.
C
C?   see if at least one track point is
C?       in the neighbourhood of the storey (1Meter)
C
      DO 10 IPT=1,NPTS
      SUM=0.
      DO 11 KC=1,3
   11 SUM=SUM+(XCENT(KC)-TXX(KC,IPT))**2
      D=SQRT(SUM)
      IF(D.LT.DISLM)GO TO 12
   10 CONTINUE
      GO TO 999
   12 CONTINUE
C
C?   see if at least one track point is
C?       in the storey
C
      KHSDA=IW(NAHSDA)
      IST=ITABL(KHSDA,ISTH,JHSDTI)
      JST=ITABL(KHSDA,ISTH,JHSDPI)
      KST=ITABL(KHSDA,ISTH,JHSDSN)
      KSC=ITABL(KHSDA,ISTH,JHSDSC)
      DO 40 I=1,NPTS
      IF(IKSCK(6,I).NE.0)GO TO 40
      IF(
     +   IST.EQ.IKSCK(1,I).AND.
     +   JST.EQ.IKSCK(2,I).AND.
     +   KST.EQ.IKSCK(3,I).AND.
     +   KSC.EQ.IKSCK(4,I))THEN
         DIST=0.
         GO TO 999
      ENDIF
   40 CONTINUE
C
C?  get theta and phi of limiting planes
C
      CALL HUSRAN(IST,JST,KST,THC,PHC,DTH,DPH)
C
C?   First index  : angle type 1 = theta 2 = phi
C?   second index : plane type 1 = lower 2 = higher
      PLANS(1,1)=THC-DTH*0.5
      PLANS(1,2)=THC+DTH*0.5
      PLANS(2,1)=PHC-DPH*0.5
      PLANS(2,2)=PHC+DPH*0.5
C
C
C?   Loop on track points
C
      DO 50 I=1,NPTS
C
C?   eliminate points outside hcal
C
      IF(IKSCK(6,I).NE.0)GO TO 50
C
C?   eliminate points in different stack or subcomponent
C
      IF(IKSCK(3,I).NE.KST.OR.IKSCK(4,I).NE.KSC)GO TO 50
C
C?  eliminate points near the entrance of medium
C
      RMS=TRTF(4,I)
      IF(RMS.GT.0.)THEN
C
C?  compute theta distance
C
         THETI=PLANS(1,1)-TRTF(2,I)
         THETS=PLANS(1,2)-TRTF(2,I)
         IF(THETI*THETS.LT.0.)THEN
            THETA=0.
         ELSE
            THETA=AMIN1(ABS(THETI),ABS(THETS))
         ENDIF
C
C?   transform dist from rads into cm
C
         DTHET=THETA*TRTF(1,I)
C
C?  compute phi distance
C
         PHII=PLANS(2,1)-TRTF(3,I)
C
C? Transform phi into its smaller value (periodic boundaries)
C
         IF(ABS(PHII).GT.PI)PHII=PHII-SIGN(TWOPI,PHII)
         PHIS=PLANS(2,2)-TRTF(3,I)
         IF(ABS(PHIS).GT.PI)PHIS=PHIS-SIGN(TWOPI,PHIS)
         IF(PHII*PHIS.LT.0.)THEN
            DPHI=0.
         ELSE
C
C?   transform dist from rads into cm
C
            DPHI=AMIN1(ABS(PHII),ABS(PHIS))*TRTF(1,I)*SIN(TRTF(2,I))
         ENDIF
         DLOC=SQRT(DTHET**2+DPHI**2)/RMS
      ELSE
         DLOC=10000.
      ENDIF
      DIST=AMIN1(DIST,DLOC)
   50 CONTINUE
  999 CONTINUE
      RETURN
      END
#endif
