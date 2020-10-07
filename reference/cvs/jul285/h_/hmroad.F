      SUBROUTINE HMROAD(ITRAC,NFPL,NEXP,NENF,NFLT,MHLT,IGFL,IEBM,ITBM)
C----------------------------------------------------------------------
C!  - Road around extrapolated track
C!
C!     Author:    F.Bossi -LNF-  3/88
C!                 revised march 1991 (bank HROA added) - G. Capon
C!                 revised march 1992 (better overlap descr.)-A.Venturi
C!
C!     Input:     ITRAC   : Track number
C!     Output:    NFPL    : Numb. of fired planes in the road
C!                NEXP    : Numb. of expected fired planes in the road
C!                NFLT    : Numb. of fired planes among last ten
C!                MHLT    : Numb. of clusters in the last ten
C!                          planes in the road.
C!                IGFL    : Geometry flag
C!                        = 0  track in active zone
C!                        = 1  dead zone between two barrel modules
C!                        = 2  dead zone between two endcap modules
C!                        = 3  barrel module notch
C!                        = 4  endcap module spacer
C!                        = 5  not active zone of tube
C!                        = 6  first plane in thin endcap
C!                        = 7  masked digital chain
C!                        = -1 error condition
C!                IEBM    : Expected bit map
C!                ITBM    : True bit map
C!                        = each bit is 1 if the corrisponding Hcal
C!                          plane is fired (ITBM), or is expected to
C!                          be fired (IEBM); otherwise is set to 0.
C!
C!       Description:
C!                   A road is opened around the extrapolated
C!                   track; its width is ROADWD times the multiple
C!                   scattering r.m.s. + 3. ( for mesurament errors and
C!                   misalignments ). The output variables are calculated
C!                   and account is taken if the particle is
C!                   expected to be near a dead zone
C!
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "ajobjj.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "rparac.h"
#include "rflags.h"
#include "trexjj.h"
#include "htubjj.h"
#include "muocut.h"
#include "hdigch.h"
#include "hroajj.h"
C
      INTEGER ALGTDB
      PARAMETER(NPLH=23)
      PARAMETER(NROMX=500)
      DIMENSION FMUL(NPLH)
      COMMON/HMGCON/NBEGTU(NPLH),SPWID1(NPLH),SPWID2(NPLH)
     +              ,NOSSP1(NPLH),NOSSP2(NPLH)
      DIMENSION IBME(NPLH),IBMT(NPLH),IPOW(NPLH)
      DIMENSION BORD(NPLH,3),BORDS(NPLH,4),ZMX(4),ZMXS(5)
      DATA IPOW/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     +          16384,32768,65536,131072,262144,524288,1048576,
     +          2097152,4194304/
      DATA ICALL/0/
#include "bmacro.h"
C
      LEN=2+LHROAA*NROMX
      CALL AUBOS('HROA',ITRAC,LEN,JHROA,IGARB)
      IF(IGARB.EQ.2) THEN
         CALL RERROR('HMROAD',1,'no space for HROA bank')
         GO TO 99
      ENDIF
      IW(JHROA+1)=LHROAA
      NHROA=0
      KHROA=JHROA+2
C
      NAJOB=NAMIND('AJOB')
      JAJOB=IW(NAJOB)
C
      NAHT=NAMIND('HTUB')
      JHTUB=IW(NAHT)
      NRHT=LROWS(JHTUB)
C
      JTREX=NLINK('TREX',ITRAC)
      NRPT=LROWS(JTREX)
C
C               load geometrical data
      IF(ICALL.EQ.0) THEN
         JDBAS=ALGTDB(LRGEOM,'HBGEHBDEHEDEHEGEHETC',
     +         IRUNRC)
         IF(JDBAS.EQ.0) THEN
            IGFL=-1
            GO TO  99
         ENDIF
         NHBGE=NAMIND('HBGE')
         JHBGE=IW(NHBGE)
         NHBDE=NAMIND('HBDE')
         JHBDE=IW(NHBDE)
         NHEDE=NAMIND('HEDE')
         JHEDE=IW(NHEDE)
         NHEGE=NAMIND('HEGE')
         JHEGE=IW(NHEGE)
         NHETC=NAMIND('HETC')
         JHETC=IW(NHETC)
C                 tube lengths for ordinary barrel modules
         ZMX(1)=RTABL(JHETC,1,12)/2.
         ZMX(2)=RTABL(JHETC,1,11)/2.
         ZMX(3)=RTABL(JHETC,1,10)/2.
         ZMX(4)=ZMX(1)
C                  tube lengths for special barrel modules
         ZMXS(1)=RTABL(JHETC,1,13)/2.
         ZMXS(2)=RTABL(JHETC,1,12)/2.
         ZMXS(3)=RTABL(JHETC,1,11)/2.
         ZMXS(4)=RTABL(JHETC,1,10)/2.
         ZMXS(5)=RTABL(JHETC,1,12)/2.
C                  barrel radius and half lenght
         RBAR=RTABL(JHBGE,1,4)
         HBLG=RTABL(JHBGE,1,6)
C                  barrel dead zones
C                  width between two barrel modules
         DWID=RTABL(JHBDE,1,10)
         DANG=ATAN2(2.*DWID,RBAR)
C                  number and position of spacers
         DO 3 IG=1,NPLH
         NBEGTU(IG)=ITABL(JHBDE,1,13+IG)
         SPWID1(IG)=RTABL(JHBDE,1,82+IG)
         SPWID2(IG)=RTABL(JHBDE,1,105+IG)
         NOSSP1(IG)=ITABL(JHBDE,1,36+IG)
         NOSSP2(IG)=ITABL(JHBDE,1,59+IG)
 3       CONTINUE
C                  end cap dead zones
C                  width between endcaps modules
         DWEC=RTABL(JHEDE,1,4)
         REEC=RTABL(JHEGE,1,11)
         DAEC=ATAN2(2.*DWEC,REEC)
C                  radius of the pole
         REPO=RTABL(JHEGE,1,10)
C                  width and distance between spacers
         SPWD=RTABL(JHEDE,1,5)
         SPDT=RTABL(JHEDE,1,6)
         XLIM = DWEC + 2.*(SPWD + SPDT)
         XMAEC=DWEC + 4.*(SPWD + SPDT)
         W8TU=RTABL(JHETC,1,5)
C           limits of the regions with different tube length
C                 in ordinary barrel modules
         DO 4 I=1,NPLH
         BORD(I,1) = DWID + 4.*W8TU
         IF(NOSSP1(I).GE.0.AND.NOSSP2(I).GE.0) THEN
             BORD(I,2) = BORD(I,1) + 4.*W8TU + SPWID1(I)
             ICK = 0
             IF((NBEGTU(I)-NOSSP2(I)).GT.1) ICK = 1
             BORD(I,3) = BORD(I,2) + FLOAT(NBEGTU(I) - 9)*W8TU +
     +                   FLOAT(ICK)*SPWID2(I)
         ELSE
             IF(NOSSP1(I).GE.0) THEN
                 IF(NOSSP1(I).LE.8) THEN
                   BORD(I,2) = BORD(I,1) + 4.*W8TU + SPWID1(I)
                   BORD(I,3) = BORD(I,2) +FLOAT(NBEGTU(I)-9)*W8TU
                 ELSEIF((NBEGTU(I)-NOSSP1(I)).GT.1) THEN
                   BORD(I,2) = BORD(I,1) + 4.*W8TU
                   BORD(I,3)=BORD(I,2)+FLOAT(NBEGTU(I)-9)*W8TU+SPWID1(I)
                 ELSE
                   BORD(I,2) = BORD(I,1) + 4.*W8TU
                   BORD(I,3)=BORD(I,2)+FLOAT(NBEGTU(I)-9)*W8TU
                 ENDIF
             ELSE
                 BORD(I,2) = BORD(I,1) + 4.*W8TU
                 BORD(I,3) = BORD(I,2) + FLOAT(NBEGTU(I) - 9)*W8TU
             END IF
         END IF
 4       CONTINUE
C               limits of regions with different tube length
C                   in barrel special modules
         DO 6 I=1,NPLH
         BORDS(I,1) = BORD(I,1) - W8TU
         BORDS(I,2) = BORD(I,1)
         BORDS(I,3) = BORD(I,2)
         BORDS(I,4) = BORD(I,3)
 6       CONTINUE
C
         ICALL=1
      ENDIF
C
      DO 5 I=1,NPLH
      IBME(I)=0
      IBMT(I)=0
 5    CONTINUE
      NFPL=0
      NFLT=0
      MHLT=0
      NEXP=0
      IGFL=0
      NENF=0
      IEBM=0
      ITBM=0
      NRLT=NRPT-10
      IF(ITABL(JTREX,NRPT,JTRELA).LT.24 .AND.
     +   (ABS(ITABL(JTREX,NRPT,JTRERG)/100).EQ.33 .OR.
     +    ABS(ITABL(JTREX,NRPT,JTRERG)/100).EQ.34)) NRLT=NRPT-9
      CALL VZERO(FMUL,NPLH)
C               loop over extrapol. points
      ICB = 0
      DO 60 I=1,NRPT
      IFLBN=0
C               region.module  and  plane
C                of the point
      IRGM1=ITABL(JTREX,I,JTRERG)
      IREGM=IRGM1/100
C                   hcal points
      IF(IRGM1.LE.2500) GO TO 60
      ICB = ICB + 1
      IF (ICB.GT.NPLH) GOTO 60
C
      IPLA=ITABL(JTREX,I,JTRELA)
      IF(IPLA.EQ.24) GO TO 60
      IBME(ICB)=1
C                expected coordin. and multiple
C                scattering r.m.s.
      XASS=RTABL(JTREX,I,JTREX3)
      YASS=RTABL(JTREX,I,JTREX3+1)
      ZASS=RTABL(JTREX,I,JTREX3+2)
      SIG=RTABL(JTREX,I,JTRERM)
C
      IARM=IABS(IREGM)
      MOD=IABS(IRGM1)-IARM*100
C
C                    end caps
C                    finds coordinate in module r.f.
      IF(IARM.EQ.33 .OR. IARM.EQ.34) THEN
         IF(IPLA.EQ.1) THEN
               IBME(ICB)=0
               GO TO 60
         END IF
         PHIP=ATAN2(YASS,XASS)
         IF(PHIP.LT.0.)PHIP=TWOPI+PHIP
         ISES=PHIP/PIBY3+1.
         XSRL=((-1)**(ISES+1))*YASS
         IF(ISES.EQ.2.OR.ISES.EQ.5)THEN
            PHIR=PIBY3
            IF(ZASS.GT.0.)THEN
               XSRL=-XASS*SIN(PHIR)+YASS*COS(PHIR)
            ELSE
               XSRL=XASS*SIN(PHIR)+YASS*COS(PHIR)
            ENDIF
            IF(ISES.EQ.5)XSRL=-XSRL
         ENDIF
         IF((FLOAT(ISES)*PIBY3-PHIP).LT.DAEC.OR.
     +   (PHIP-FLOAT((ISES-1))*PIBY3).LT.DAEC) THEN
               IGFL=2
               NENF=NENF+1
               IBME(ICB)=0
               GO TO 40
         ENDIF
         IF(XSRL.GT.XMAEC) THEN
               IGFL=2
               NENF=NENF+1
               IBME(ICB)=0
               GOTO 40
         ENDIF
         IF(XSRL.LT.DWEC) THEN
               IGFL=2
               NENF=NENF+1
               IBME(ICB)=0
               GOTO 40
         ENDIF
         DO 10 ISP=1,4
         IF(XSRL.LE.(FLOAT(ISP)*(SPDT+SPWD)+DWEC).AND.
     +      XSRL.GE.(FLOAT(ISP)*(SPDT+SPWD)-SPWD+DWEC)) THEN
               IGFL=4
               NENF=NENF+1
               IBME(ICB)=0
               GO TO 40
         ENDIF
 10      CONTINUE
         IF((XASS**2+YASS**2).GT.(REPO**2)) THEN
           IF(IPLA.LE.8) THEN
             IGFL=6
             NENF=NENF+1
             IBME(ICB)=0
             GOTO 40
           ENDIF
         ENDIF
         IF(ZASS.GT.0.) THEN
              IF(XSRL.GT.XLIM) THEN
                   IVET = ISES + 6
              ELSE
                   IVET = ISES
              END IF
         ELSE
              IF(XSRL.GT.XLIM) THEN
                   IVET = ISES + 30
              ELSE
                   IVET = ISES + 24
              END IF
         END IF
         IF(MASK(IVET,IPLA).EQ.0) THEN
            IGFL=7
            IBME(ICB) = 0
            NENF = NENF + 1
            GO TO 40
         END IF
C
C                    barrel
C                    finds coordinate in module r.f.
      ELSEIF(IARM.EQ.31 .OR. IARM.EQ.32) THEN
         PHIM=FLOAT(MOD-1)*PIBY6
         PHIR=PHIM-PIBY2
         XSRL=(XASS-RBAR*COS(PHIM))*COS(PHIR)
         XSRL=XSRL+(YASS-RBAR*SIN(PHIM))*SIN(PHIR)
         IF(XSRL.GE.0.) THEN
            NMTX = MOD*2 - 2
            IF(NMTX.EQ.0) NMTX = 24
         ELSE
            NMTX = MOD*2 - 1
         END IF
         XSRL=ABS(XSRL)
C
         IF(XSRL.LE.DWID) THEN
                          IGFL=1
                          NENF=NENF+1
                          IBME(ICB)=0
                          GO TO 40
                          END IF
         XMAX=FLOAT(NBEGTU(IPLA))*W8TU+SPWID1(IPLA)+SPWID2(IPLA)+DWID
         IF(XSRL.GE.XMAX)THEN
               IGFL=1
               NENF=NENF+1
               IBME(ICB)=0
               GO TO 40
         END IF
         IF(IARM.EQ.32)THEN
              IGFL=3
              NENF=NENF+1
              IBME(ICB)=0
              GO TO 40
         ENDIF
         IVET = (NMTX + 1)/2 + 12
         IF(MASK(IVET,IPLA).EQ.0) THEN
            IGFL=7
            IBME(ICB) = 0
            NENF = NENF + 1
            GO TO 40
         END IF
         IF(NMTX.EQ.6.OR.NMTX.EQ.7) THEN
C              check if the extr. point is in tube active zone
C                     of barrel special module
           ZTU = ZMXS(5)
           DO 37 IA = 4,1,-1
           IF(XSRL.LT.BORDS(IPLA,IA)) ZTU = ZMXS(IA)
 37        CONTINUE
           IF(ABS(ZASS).GT.ZTU) THEN
C   Set flag for big notch problem in MC
              IF(ABS(ZTU-ZMXS(1)).LT.1.) IFLBN=1
              IGFL=3
              NENF = NENF + 1
              IBME(ICB) = 0
              GO TO 40
           END IF
         ELSE
C              check if the extr. point is in tube active zone
C                  of barrel ordinary module
           ZTU = ZMX(4)
           DO 38 IA = 3,1,-1
           IF(XSRL.LT.BORD(IPLA,IA)) ZTU = ZMX(IA)
 38        CONTINUE
           IF(ABS(ZASS).GT.ZTU) THEN
              IGFL=3
              NENF = NENF + 1
              IBME(ICB) = 0
              GO TO 40
           END IF
         ENDIF
      ELSE
         GO TO 60
      ENDIF
C
 40   NEXP=NEXP+1
C                    loop over hcal clusters
      DO 50 IM=1,NRHT
C  Check if it is MC, old Galeph version and extrap. in the Big Notch
      IF(JAJOB.NE.0) THEN
        MCVER=ITABL(JAJOB,1,JAJOGV)
        MCCOR=ITABL(JAJOB,1,JAJOCV)
        XVER=FLOAT(MCVER)+FLOAT(MCCOR)/10000.
        IF(XVER.LT.251.AND.IFLBN.EQ.1) GOTO 50
      ENDIF
      ISUB=ITABL(JHTUB,IM,JHTUSN)
C                    module and plane of cluster
      MDST=ITABL(JHTUB,IM,JHTUMN)
      LAYT=ITABL(JHTUB,IM,JHTULN)
      IF(IARM.EQ.33 .OR. IARM.EQ.34)LAYT=LAYT+1
C
C                   looks for cluster with
C                   same module nb. and same
C                   plane of the point
      IF(LAYT.NE.IPLA) GO TO 50
C                    barrel clusters
      IF(ISUB.EQ.2) THEN
         IF(IARM.NE.31 .AND. IARM.NE.32) GO TO 50
         IF(MDST.NE.NMTX) GO TO 50
      ELSE
C                    end cap clusters
         IF(IARM.NE.33 .AND. IARM.NE.34) GO TO 50
         IF((ISUB.EQ.1.AND.ZASS.LT.0.).OR.(ISUB.EQ.3.AND.ZASS.GT.0.))
     +     GO TO 50
         IF(MDST.NE.ISES) GO TO 50
      ENDIF
C                    comparison between cluster
C                    and point coordinate
C                    coord. of cluster in module
C                    reference frame
      XLOC=RTABL(JHTUB,IM,JHTULC)
C                    width of cluster
      WCLU=RTABL(JHTUB,IM,JHTUCW)
C
      DIFX=XLOC-XSRL
      CONEW=21+(I-NRLT)
      KFLAG=0
      IF(I.GE.(NRLT-1) .AND. ABS(DIFX).LT.CONEW)THEN
         FMUL(ICB)=FMUL(ICB)+1.
         KFLAG=KFLAG+2
      ENDIF
      DIFN=DIFX/SIG
      IF(JHISRF(JULHC).GE.1) CALL HFILL(7120,DIFN,0.,1.)
C                    cluster inside the road
      IF(WCLU.LE.WCLUMX .AND. ABS(DIFX).LE.(ROADWD*SIG+3.)) THEN
         KFLAG=KFLAG+1
         IF(I.GE.NRLT) KFLAG=KFLAG+4
         IF(IBMT(ICB).EQ.0)THEN
            IBMT(ICB)=1
            NFPL=NFPL+1
            IF(I.GE.NRLT)NFLT=NFLT+1
         ENDIF
      ENDIF
C
      IF(KFLAG.GT.0) THEN
         RW(KHROA+JHRODX)=DIFX
         RW(KHROA+JHROSG)=SIG
         IW(KHROA+JHROSF)=KFLAG+256*ICB
         IW(KHROA+JHROHI)=IM
         IF (NHROA.LT.NROMX) THEN
            NHROA=NHROA+1
            KHROA=KHROA+LHROAA
         ENDIF
      ENDIF
C
 50   CONTINUE
 60   CONTINUE
C
      SFMUL=0.
      FIRED=0.
      DO 81 LLAY=1,23
      IF (FMUL(LLAY).GT.0.) THEN
         FIRED=FIRED+1.
         SFMUL=SFMUL+FMUL(LLAY)
      ENDIF
 81   CONTINUE
      IF(FIRED.GE.4.) MHLT =NINT((100.*SFMUL)/FIRED)
      IW(JHROA+2)=NHROA
      LEN=LHROAA*NHROA+2
      CALL AUBOS('HROA',ITRAC,LEN,JHROA,IGARB)
      DO 85 JB=1,NPLH
      IEBM=IEBM+IBME(JB)*IPOW(JB)
      ITBM=ITBM+IBMT(JB)*IPOW(JB)
 85   CONTINUE
C
 99   CONTINUE
C
      RETURN
      END
C
#endif
