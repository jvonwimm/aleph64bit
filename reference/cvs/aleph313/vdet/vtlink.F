      SUBROUTINE  VTLINK(ITK)
C-------------------------------------------------------------
C!Track link with vertex detector clusters
CKEY VDET TRACK
C
C  Author      : B. Mours     901001
C  modified by : B, Mours     910918
C     adjust error for large pulses or double pulses
C  modified by : B, Mours     911023
C     do not associate half VDET hit to track without ITC hit
C  modified by : B, Mours     920213
C     change the definition of large pulses for increased errors
C  modified by : D Brown,   920927
C     Small change in logic for large pulseheight error assignment
C  input is the track number . (in FRFT bank)
C   Use extrapolated TPC+ITC tracks to the VDET. Do Track cluster
C   association and fill the VTMA bank.
C  modified by : A. Bonissent  950714
C     Use (year sensitive) subroutine to check that hits
C     in the two views are in the same wafer/module
C
C
C-------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "vtkrec.h"
#include "vtrpar.h"
#include "vtmajj.h"
#include "vtscjj.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vtucjj.h"
#include "vtwcjj.h"
#include "frtljj.h"
#include "vtxtjj.h"
C
#include "vtbos.h"
C
C  Local variables
C
      INTEGER NVTUC,NVTWC,KVTXT,JVTUC,JVTWC,JVTXT,JVTMA,KVTMA,NVTMA
      INTEGER IL,ICOMB,NXY,NZT,NCOMB,IZT,ICLSU,ICLSW,IDWFU,IDWW,NWAF
     +       ,NU,NW,NUW
      REAL    SIGMU,SIGMW,VUW(3),XYZ(3),SU(4),SW(4)
      REAL PCOR(4)
C - bit 0 (ISEPBT=1) is set in the VDXY and VDZT quality flag
C   to indicate a separated hit
      INTEGER ISEPBT
      PARAMETER (ISEPBT=1)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C  Include explicitly the JULIA *CD BOSEXT 5-2-91 D.BROWN
C
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
#include "bmacro.h"
C
C - 1st entry
C
      IF (FIRST) THEN
         NVTMA = NAMIND('VTMA')
         NFRTL = NAMIND('FRTL')
         FIRST = .FALSE.
      ENDIF
C
C - reset the number og good clusters to 0
C
      KVTMA = IW(NVTMA)
      IW(KVTMA+LMHROW) = 0
C
C-- check if Extrapolation bank there for this track
C
      IF(NLINK('VTXT',ITK).EQ.0) GO TO 999
      IF(NLINK('VTER',ITK).EQ.0) GO TO 999
C
C
      CALL VZERO (IW(KVTUC+LMHCOL+1),IW(KVTUC)-LMHCOL)
      CALL VZERO (IW(KVTWC+LMHCOL+1),IW(KVTWC)-LMHCOL)
      CALL VZERO (IW(KVTS0+LMHCOL+1),IW(KVTS0)-LMHCOL)
      CALL VZERO (IW(KVTS1+LMHCOL+1),IW(KVTS1)-LMHCOL)
C
C-- do cluster association in U and W plan separetly
C
      CALL VTCLAS(ITK,0,NXY)
      CALL VTCLAS(ITK,1,NZT)
C
C-- do the space association: Loop over all possible U/W combinaisons,
C   compute the overall chisquare and store the good one in VTMA.
C   (remember: the last combinaison IXY=NXY & IZT=NZT is empty,
C   so we don't load it)
C
      KFRTL = IW(NFRTL)
      NITC  = ITABL(KFRTL,ITK,JFRTNI)
      KVTMA = IW(NVTMA)
      NCOMB = NXY*NZT-1
      IW(KVTMA+LMHROW) = 0
      IF(NCOMB.LE.0)             GO TO 999
      IF(NCOMB.GT.MAXCOM*MAXCOM) GO TO 999
      CALL VZERO (IW(KVTMA+LMHLEN+1),NCOMB*LVTMAA)
C
      KVTXT = NLINK('VTXT',ITK)
      DO 50 IL=1,NLAYER
         JVTXT = KROW(KVTXT,IL)
         CALL VDHTER(IW(JVTXT+JVTXWI),RW(JVTXT+JVTXPV),
     +               RW(JVTXT+JVTXPU),RW(JVTXT+JVTXPW),
     +               RW(JVTXT+JVTXUC),RW(JVTXT+JVTXWC),
     +               USNOIS,WSNOIS,SU(IL),SW(IL))
         PTOT = SQRT(RW(JVTXT+JVTXPV)**2 +
     +               RW(JVTXT+JVTXPU)**2 +
     +               RW(JVTXT+JVTXPW)**2 )
         PCOR(IL) =  ABS(RW(JVTXT+JVTXPV)) / PTOT
         JVTXT = JVTXT + LCOLS(KVTXT)
  50  CONTINUE
C
      DO 200 IXY = 1,NXY
        DO 200 IZT = 1,NZT
          IF(IXY.EQ.NXY .AND. IZT.EQ.NZT) GO TO 200
C
C-- check if the XY and ZT wafer are the same
C (remember there is only 2 wafers address in z for yx but 4 for zt)
C
          DO 100 IL = 1,NLAYER
            ICLSU = IW(KROW(KVTS0,IXY)+JVTSCI+IL-1)
            ICLSW = IW(KROW(KVTS1,IZT)+JVTSCI+IL-1)
            IF(RW(KROW(KVTUC,ICLSU)+JVTUSU+IL-1).GT.HBIGER) GO TO 100
            IF(RW(KROW(KVTWC,ICLSW)+JVTWSW+IL-1).GT.HBIGER) GO TO 100
            IDWFU = IW(KROW(KVTUC,ICLSU)+JVTUWI+IL-1)
            IDWFW = IW(KROW(KVTWC,ICLSW)+JVTWWI+IL-1)
C
C Check that u- and w-cluster are in the same module,
C VRMWF transforms the wafer identifier into a u module identifier.
C
            CALL VRMWF(IDWFW,2,IROM)
            IF(IDWFU.NE.IROM) GO TO 200
  100     CONTINUE
C
C
C-- remove track with only half a VDET cluster to reduce missaciation
C   (we cut a harder for track without ITC hits to remove conversion)
C
          IF(NLAYER.GE.2) THEN
            NU  = 0
            NW  = 0
            NUW = 0
            DO 130 IL = 1,NLAYER
              ICLSU = IW(KROW(KVTS0,IXY)+JVTSCI+IL-1)
              ICLSW = IW(KROW(KVTS1,IZT)+JVTSCI+IL-1)
              SIGMU = RW(KROW(KVTUC,ICLSU)+JVTUSU+IL-1)
              SIGMW = RW(KROW(KVTWC,ICLSW)+JVTWSW+IL-1)
              IF(SIGMU.LT.HBIGER) NU = NU+1
              IF(SIGMW.LT.HBIGER) NW = NW+1
              IF(SIGMU.LT.HBIGER .AND.
     +           SIGMW.LT.HBIGER) NUW = NUW+1
  130       CONTINUE
            IF(NU+NW.LE.1) GO TO 200
            IF(NITC.EQ.0 .AND. NUW.LE.0 .AND. MAX(NU,NW).LE.1) GO TO 200
          ENDIF
C
C-- load cluster values into VTMA bank
C
          ICOMB = LROWS(KVTMA) + 1
          IW(KVTMA+LMHROW) = ICOMB
          JVTMA = KROW(KVTMA,ICOMB)
          IW(JVTMA+JVTMNL) = NLAYER
          IW(JVTMA+JVTMNU) = 0
          IW(JVTMA+JVTMNW) = 0
          RW(JVTMA+JVTMC2) = 0.
          IW(JVTMA+JVTMIT) = ITK
          DO 150 IL=1,NLAYER
            ICLSU = IW(KROW(KVTS0,IXY)+JVTSCI+IL-1)
            ICLSW = IW(KROW(KVTS1,IZT)+JVTSCI+IL-1)
            JVTUC = KROW(KVTUC,ICLSU)
            JVTWC = KROW(KVTWC,ICLSW)
            IW(JVTMA+JVTMUW+IL-1) = IW(JVTUC+JVTUWI+IL-1)
            IW(JVTMA+JVTMWW+IL-1) = IW(JVTWC+JVTWWI+IL-1)
            IW(JVTMA+JVTMIU+IL-1) = IW(JVTUC+JVTWCI+IL-1)
            IW(JVTMA+JVTMIW+IL-1) = IW(JVTWC+JVTUCI+IL-1)
C
C-- convert U,W to x,y,z using alignement
C
            NWAF = IW(JVTMA+JVTMWW+IL-1)
            VUW(1) = 0.
            VUW(2) = RW(JVTUC+JVTUUC+IL-1)
            VUW(3) = RW(JVTWC+JVTWWC+IL-1)
            CALL VGWFXY(IW(JVTMA+JVTMWW+IL-1),VUW,XYZ)
            RW(JVTMA+JVTMUC+IL-1) = VUW(2)
            RW(JVTMA+JVTMWC+IL-1) = VUW(3)
            RW(JVTMA+JVTMR0+IL-1) = SQRT(XYZ(1)**2+XYZ(2)**2)
            RW(JVTMA+JVTMPH+IL-1) = ATAN2(XYZ(2),XYZ(1))
            RW(JVTMA+JVTMZ0+IL-1) = XYZ(3)
C
            SIGMU = RW(JVTUC+JVTUSU+IL-1)
            SIGMW = RW(JVTWC+JVTWSW+IL-1)
            IF(SIGMU.LT.HBIGER) SIGMU = SU(IL)
            IF(SIGMW.LT.HBIGER) SIGMW = SW(IL)
C
C           Check if we should increase U error
C           (for double pulse or too big pulses)
C
            PULSU = 0.
            IWAF = IW(JVTUC+JVTUWI+IL-1)
            ICL  = IW(JVTUC+JVTUCI+IL-1)
            KVDXY = NLINK('VDXY',IWAF)
            IF(KVDXY.NE.0 .AND. ICL.GT.0) THEN
              IQFLG = ITABL(KVDXY,ICL,JVDXQF)
              IF(IAND(IQFLG,ISEPBT).NE.0) SIGMU = ESPLP2
              PULSU = RTABL(KVDXY,ICL,JVDXPH)*PCOR(IL)
            ENDIF
C
C          Check if we should increase W error
C
            PULSW = 0.
            IWAF = IW(JVTWC+JVTWWI+IL-1)
            ICL  = IW(JVTWC+JVTWCI+IL-1)
            KVDZT = NLINK('VDZT',IWAF)
            IF(KVDZT.NE.0 .AND. ICL.GT.0) THEN
              IQFLG = ITABL(KVDZT,ICL,JVDZQF)
              IF(IAND(IQFLG,ISEPBT).NE.0) SIGMW = ESPLP2
              PULSW = RTABL(KVDZT,ICL,JVDZPH)*PCOR(IL)
            ENDIF
C
            IF(ABS(PULSU-PULSW).GT..35*PULMIN .AND.
     +         MIN(PULSU,PULSW).GT.1..AND.
     +         MAX(PULSU,PULSW).GT.PULMIN) THEN
C
C  Pulseheights don't match, and at least 1 is large;  Increase
C  the error for the larger pulseheight hit as for a double hit.
C
              IF(PULSU.GT.PULSW)THEN
                SIGMU = ELARP2
              ELSE
                SIGMW = ELARP2
              END IF
            ENDIF
C
            RW(JVTMA+JVTMSU+IL-1) = SIGMU
            RW(JVTMA+JVTMSW+IL-1) = SIGMW
            IF(SIGMU.LT.HBIGER) IW(JVTMA+JVTMNU) = IW(JVTMA+JVTMNU)+1
            IF(SIGMW.LT.HBIGER) IW(JVTMA+JVTMNW) = IW(JVTMA+JVTMNW)+1
            RW(JVTMA+JVTMCO+IL-1) = 0.
 150      CONTINUE
C
 200    CONTINUE
C
C
 999  CONTINUE
      RETURN
C
      END
#endif
