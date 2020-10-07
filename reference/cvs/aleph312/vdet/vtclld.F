      SUBROUTINE  VTCLLD(ITK,IL,SCUT,IUORW,NCL)
C-------------------------------------------------------------
C!Return VDET cluster whitin an given area.
CKEY VDET TRACK
C
C  Author     :  B. Mours - 901001
C  modified by : B. Mours - 910918
C    look for cluster in adjacent wafers
C                H.G. Moser - 910918
C    normelise pulseheight before cut, select good clusters
C
C                A. Bonissent March 1995
C          use Vdet year sensitive routime VRMWF
C          to go from wafer to readout module
C          New logic for multitrack assignment and new Vdet
C          (old Vdet unchanged)
C
C  We neglect the error comming from the cluster.
C  This routine give at less a dummy cluster (residual=0, sigma=0)
C
C    ITK   (in)  Track number
C    IL    (in)  Layer number
C    SCUT  (in)  Size of the search area
C    IUORW (in)  = 0 if in xy plan; 1 if in z
C    NCL   (out) Number of found clusters + 1 (dummy cluster)
C-------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "vtkrec.h"
#include "vtrpar.h"
C
C   NRR = NR for appropriate view (view included)
      INTEGER NRR,IHIT
      INTEGER NVTUC,NVTWC,KVTXT,KVDXZ,KVTXC,ICL,NASS,JVTXC,IWAF
      REAL    RES,PULSE
      REAL VUW(2),XYZ(3),VEC(3),RERRP(10)
      INTEGER VDYEAR
      INTEGER IERR,IWAF0
      INTEGER IROM,IVIEW
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      LOGICAL FIRSTW
C - bit 1 (IVETOC=2) is set in VDXY and VDZT quality flag to indicate
C   a generic veto hit.
C   bit 30 (IMCNEF=536870912) is set in VDXY and VDZT quality flag
C   to indicate a MC inefficiency rejected hit.
      INTEGER IVETOC,IMCNEF
      PARAMETER (IVETOC=2, IMCNEF=536870912)
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vdgcjj.h"
#include "vdmrjj.h"
#include "frftjj.h"
#include "vtucjj.h"
#include "vtwcjj.h"
#include "vtxtjj.h"
#include "vtbos.h"
#include "bcs.h"
#include "bmacro.h"
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDGC=NAMIND('VDGC')
      ENDIF
      KVTXT = NLINK('VTXT',ITK)
      IF(KVTXT.EQ.0) GO TO 999
C
      NCL = 1
      IWAF = ITABL(KVTXT,IL,JVTXWI)
      FIRSTW = .TRUE.
      IMASK = IVETOC+IMCNEF
      VUW(1) = RTABL(KVTXT,IL,JVTXUC)
      VUW(2) = RTABL(KVTXT,IL,JVTXWC)
C
   50 CONTINUE
C
      IVIEW=2-IUORW
      CALL VRMWF(IWAF,IVIEW,IROM)
      IWAF=IROM
      IF(IUORW.EQ.0) THEN
        KVDXZ = NLINK('VDXY',IWAF)
        KVTXC = KVTUC
        NRR=IWAF+1
      ELSE
        KVDXZ = NLINK('VDZT',IWAF)
        KVTXC = KVTWC
        NRR=IWAF
      ENDIF
      IF(KVDXZ.EQ.0) GO TO 900
C
C     pulseheight correction factor for inclined tracks
C
      PTOT = SQRT( RTABL(KVTXT,IL,JVTXPV)**2 +
     +             RTABL(KVTXT,IL,JVTXPU)**2 +
     +             RTABL(KVTXT,IL,JVTXPW)**2 )
      PCOR = ABS(RTABL(KVTXT,IL,JVTXPV)) / PTOT
C
C-- Loop over all cluster, keep only the close one
C
      DO 100 ICL = 1,LROWS(KVDXZ)
        JVTXC = KROW(KVTXC,NCL)
        IF(IUORW.EQ.0) THEN
          RES   = RTABL(KVDXZ,ICL,JVDXUC) - VUW(1)
          NASS  = ITABL(KVDXZ,ICL,JVDXNA)
          PULSE = RTABL(KVDXZ,ICL,JVDXPH) * PCOR
          IQFL  = ITABL(KVDXZ,ICL,JVDXQF)
          RW(JVTXC+JVTURE+IL-1) = RES
        ELSE
          RES   = RTABL(KVDXZ,ICL,JVDZWC) - VUW(2)
          NASS  = ITABL(KVDXZ,ICL,JVDZNA)
          PULSE = RTABL(KVDXZ,ICL,JVDZPH) * PCOR
          IQFL  = ITABL(KVDXZ,ICL,JVDZQF)
          RW(JVTXC+JVTWRE+IL-1) = RES
        ENDIF
        IF(IAND(IQFL,IMASK).NE.0)       GO TO 100
        IF(ABS(RES).GT.SCUT)            GO TO 100
        IF(VDYEAR().NE.95)THEN
           IF(PULSE.LT.FLOAT(NASS)*PULMIN) GO TO 100
        ELSE
C
C NASS<0 means that tracks have already been associated but
C pulseheight is large enough for one more track
C Therefore we only need to test against >0, which means no more
C tracks can be given
C This is set in VTFILL
C
           IF(NASS.GT.0)GO TO 100
           KVDGC=IW(NAVDGC)
           KVDMR=NLINK('VDMR',NRR)
C Find the relation to global cluster
           IVDGC=ITABL(KVDMR,ICL,JVDMVD)
           NASG =ITABL(KVDGC,IVDGC,JVDGNA)
           IF(NASG.GT.0)GO TO 100
        ENDIF
C
        IF(NCL.GE.MAXCLS) GO TO 999
        IW(JVTXC+JVTUWI+IL-1) = IWAF
        IW(JVTXC+JVTUCI+IL-1) = ICL
        IF(IUORW.EQ.0) THEN
          RW(JVTXC+JVTURC+IL-1) = SQRT( RTABL(KVDXZ,ICL,JVDXXC)**2+
     +                                  RTABL(KVDXZ,ICL,JVDXYC)**2)
          RW(JVTXC+JVTUPH+IL-1) = ATAN2(RTABL(KVDXZ,ICL,JVDXYC),
     +                                  RTABL(KVDXZ,ICL,JVDXXC))
          RW(JVTXC+JVTUUC+IL-1) =       RTABL(KVDXZ,ICL,JVDXUC)
          RW(JVTXC+JVTUSU+IL-1) =       RTABL(KVDXZ,ICL,JVDXSU)**2
        ELSE
          RW(JVTXC+JVTWZC+IL-1) = RTABL(KVDXZ,ICL,JVDZZC)
          RW(JVTXC+JVTWWC+IL-1) = RTABL(KVDXZ,ICL,JVDZWC)
          RW(JVTXC+JVTWSW+IL-1) = RTABL(KVDXZ,ICL,JVDZSW)**2
        ENDIF
        NCL = NCL+1
  100 CONTINUE
C
  900 CONTINUE
C
C-- look for cluster in the adjacent z wafer (only if xy)
C
      IF(FIRSTW) IWAF0 = IWAF
      IF(ITABL(KVTXT,IL,JVTXHF).NE.0 .AND. FIRSTW) THEN
        FIRSTW = .FALSE.
        KFRFT = NLINK('FRFT',0)
        IWAFO = ITABL(KVTXT,IL,JVTXWI)
        IF(VUW(2).LT.0.) THEN
          IWAF = IWAFO-1000
        ELSE
          IWAF = IWAFO+1000
        ENDIF
        IZED  = MOD(IWAF/1000,10)
        IZEDO = MOD(IWAFO/1000,10)
C
C The definition of neighbouring wafers is different for new VDET
C
        IF(VDYEAR().NE.95)THEN
          IF(IWAF.GE.0 .AND. IZED.LE.3 .AND.
     +      (IUORW.NE.0 .OR. (IZED-1.5)*(IZEDO-1.5).LT.0.)) THEN
            CALL VTXNWT(IWAF,RW(KROW(KFRFT,ITK)+JFRFIR),
     +                     RW(KROW(KVTXT,IL)+JVTXXC),
     +                  VUW,XYZ,VEC,RERRP,IERR)
            IF(IERR.EQ.0) GO TO 50
          ENDIF
        ELSE
C This is new VDET. We always look in adjacent wafer.
C  5 is (number of wafers in a face)-1
          IF(IWAF.GE.0.AND.IZED.GE.0 .AND. IZED.LE.5) THEN
            CALL VTXNWT(IWAF,RW(KROW(KFRFT,ITK)+JFRFIR),
     +                     RW(KROW(KVTXT,IL)+JVTXXC),
     +                  VUW,XYZ,VEC,RERRP,IERR)
            IF(IERR.EQ.0) GO TO 50
          ENDIF
        ENDIF
      ENDIF
      IWAF = IWAF0
C
C  Fill dummy cluster
C
      JVTXC = KROW(KVTXC,NCL)
      IW(JVTXC+JVTUWI+IL-1) = IWAF
      IW(JVTXC+JVTUCI+IL-1) = 0
      IF(IUORW.EQ.0) THEN
        RW(JVTXC+JVTURE+IL-1) = 0.
        RW(JVTXC+JVTURC+IL-1) = SQRT( RTABL(KVTXT,IL,JVTXXC)**2+
     +                                RTABL(KVTXT,IL,JVTXYC)**2)
        RW(JVTXC+JVTUPH+IL-1) = ATAN2(RTABL(KVTXT,IL,JVTXYC),
     +                                RTABL(KVTXT,IL,JVTXXC))
        RW(JVTXC+JVTUUC+IL-1) =       RTABL(KVTXT,IL,JVTXUC)
        RW(JVTXC+JVTUSU+IL-1) = BIGERR
      ELSE
        RW(JVTXC+JVTWRE+IL-1) = 0.
        RW(JVTXC+JVTWZC+IL-1) = RTABL(KVTXT,IL,JVTXZC)
        RW(JVTXC+JVTWWC+IL-1) = RTABL(KVTXT,IL,JVTXWC)
        RW(JVTXC+JVTWSW+IL-1) = BIGERR
      ENDIF
C
  999 CONTINUE
      IW(KVTXC+LMHROW) = MAX(NCL,IW(KVTXC+LMHROW))
C
      RETURN
      END
#endif
