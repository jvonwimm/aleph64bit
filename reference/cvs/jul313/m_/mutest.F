      SUBROUTINE MUTEST (ITRAC,C0,C1,C2,IASS)
C-------------------------------------------------------------
C
C! test mu-hits within mult. scatt. cuts
C!
C!   G.Capon, M.Dreucci        28-May-87
C!  input banks : MHIT     output banks : MCAD
C!
C!   Input _ :    ITRAC  =  Track no.
C!             C0,C1,C2  =  coefficients to compute radial cuts DCUT
C!                          and angular cut ACUT
C!    Output :    IASS   =  Flag : 0 = track not assoc.
C!                                 1 = track assoc.
C?
C!======================================================================
#ifndef DOC
      PARAMETER (NAMX=30)
C         NAMX = max nb of hits associated to a track !!
      PARAMETER (XRES2=(1+1.44)/24.)
      DIMENSION  DVF(3),NASS(2),XX(2,NAMX),YY(2,NAMX),
     * ZZ(2,NAMX),DISMI(2),DCUTM(2),DAVGM(2),NSUBC(2,NAMX)
      DIMENSION PARIN(6),PAREX(6)
#include "alcons.h"
#include "bcs.h"
#include "muocut.h"
#include "munamc.h"
#include "mujosu.h"
#include "mcadjj.h"
#include "mthrjj.h"
#include "mhitjj.h"
#include "rlunit.h"
#include "rparac.h"
#include "mdebug.h"
#include "mwbank.h"
#include "muexjj.h"
C                   average pitch of X and Y strips !!
      DATA PITCH /1.1/
#include "bmacro.h"
C
      JMHIT= IW(NAMHIT)
      NMHIT= IW(JMHIT+2)
      JMUEX=NLINK('MUEX',ITRAC)
      IF(JMUEX.LE.0) GO TO 99
      JMTHR=NLINK('MTHR',0)
      LRMEM=LROWS(JMTHR)
C                  get momentum, coordinates at Hcal exit point
      DVF(1)=RTABL(JMUEX,1,JMUEXE)
      DVF(2)=RTABL(JMUEX,1,JMUEXE+1)
      DVF(3)=RTABL(JMUEX,1,JMUEXE+2)
      PX=RTABL(JMUEX,1,JMUEPE)
      PY=RTABL(JMUEX,1,JMUEPE+1)
      PZ=RTABL(JMUEX,1,JMUEPE+2)
      C0=RTABL(JMUEX,1,JMUEC0)
      C1=RTABL(JMUEX,1,JMUEC1)
      C2=RTABL(JMUEX,1,JMUEC2)
      PEXIT=SQRT(PX*PX+PY*PY+PZ*PZ)
      ALFA  =  PX/PEXIT
      BETA  =  PY/PEXIT
      GAMMA =  PZ/PEXIT
C
      CALL VZERO (NASS,2)
      IASS   = 0
      DISMI(1) = 999.
      DISMI(2) = 999.
      DCUTM(1) = 0.
      DCUTM(2) = 0.
C
C?         loop on muon ch hits - compute hit-track distance
C
      DO 10 I=1,NMHIT
      YLHIT=RTABL(JMHIT,I,JMHIYL)
      XH = RTABL(IWBANK,I,1)
      YH = RTABL(IWBANK,I,2)
      ZH = RTABL(IWBANK,I,3)
      IE    =  ITABL(IWBANK,I,4)
      DEXIT = ALFA*(XH-DVF(1)) + BETA*(YH-DVF(2)) + GAMMA*(ZH-DVF(3))
C                     skip hits in opposite hemisphere !!
      IF (DEXIT.LE.0.) GO TO 10
      XE = DVF(1) + ALFA*DEXIT
      YE = DVF(2) + BETA*DEXIT
      ZE = DVF(3) + GAMMA*DEXIT
      DISQ=C0+C1*DEXIT+C2*DEXIT*DEXIT
      DAVG = SQRT(PIBY2*( DISQ + PITCH*PITCH/12.))
      DCUT = ACCLAY(IE)*DAVG
C                          put a lower limit on DCUT to account for
C                          chamber misalignment. G. Capon sept 1991
      IF (DCUT.LT.5.) DCUT=5.
      DIST  =  SQRT ((XH-XE)**2+(YH-YE)**2+(ZH-ZE)**2)
C                     find closest hit
      IF(DIST.LT.DISMI(IE)) THEN
          DISMI(IE)=DIST
          DCUTM(IE)=DCUT
          DAVGM(IE)=DAVG
          ENDIF
C
C?         if DIST < cut, the hit is associated
C
      IF(DIST.LT.DCUT) THEN
C                 check whether space left in MTHR, otherwise
C                 enlarge   -   28-Oct-1991, H. Meinhard
           IF (LFRROW(JMTHR) .EQ. 0) THEN
               CALL AUBOS('MTHR',0,IW(JMTHR)+LCOLS(JMTHR),JMTHR,IGARB)
               IF (JMTHR .EQ. 0 .OR. IGARB .EQ. 2) THEN
                   CALL RERROR('MUTEST',1,
     +               'No space for MTHR enlargement')
                   GOTO 99
               ENDIF
               IF (IGARB .EQ. 1) THEN
                   JMHIT = IW(NAMHIT)
                   JTREX = NLINK('TREX',ITRAC)
               ENDIF
           ENDIF
           KMTHR = KNEXT(JMTHR)
           IW(KMTHR+JMTHHN)=I
           RW(KMTHR+JMTHDD)=DIST/DCUT
           IW(KMTHR+JMTHTN)=ITRAC
           IW(JMTHR+2)=IW(JMTHR+2)+1
           IASS = 1
           IF(NASS(IE).LT.NAMX) THEN
               NASS(IE)=NASS(IE)+1
               XX (IE,NASS(IE))   =  XH
               YY (IE,NASS(IE))   =  YH
               ZZ (IE,NASS(IE))   =  ZH
               NSUBC(IE,NASS(IE)) =  ITABL(IWBANK,I,5)
               ENDIF
      ENDIF
 10   CONTINUE
C
C                 fill histos, debug printing
C
      DO 33 IE=1,2
      IF(DCUTM(IE). GT. 0.)THEN
         CALL HFILL(8006+IE,DISMI(IE),0.,1.)
         CALL HFILL(8008+IE,DAVGM(IE),0.,1.)
         DNORM=DISMI(IE)/DAVGM(IE)
         CALL HFILL(8005,DNORM,0.,1.)
         R02=DAVGM(IE)*DAVGM(IE)/PIBY2
         CHI2=DISMI(IE)*DISMI(IE)/R02
         PCHI2=PROB(CHI2,2)
         CALL HFILL(8011+IE,PCHI2,0.,1.)
      ENDIF
 33   CONTINUE
      IF(IASS.GT.0.AND.IMPRIN.GE.1) THEN
      IF(NASS(1).GT.0) WRITE(LDEBRL,76) NASS(1),ITRAC,DISMI(1),DCUTM(1)
      IF(NASS(2).GT.0) WRITE(LDEBRL,77) NASS(2),ITRAC,DISMI(2),DCUTM(2)
      ENDIF
C
C?         if int and ext chambers associated : compute exit angle
C?         exit angle = angle between expected and observed exit direction
C
      ACUT=0.
      ITEXI=0
      IF(NASS(1)*NASS(2).GT.0) THEN
           THSQ = C2
           ANGMI=999.
           DO 20 N1=1,NASS(1)
           DO 20 N2=1,NASS(2)
           D1 = XX(2,N2)-XX(1,N1)
           D2 = YY(2,N2)-YY(1,N1)
           D3 = ZZ(2,N2)-ZZ(1,N1)
           DD = SQRT (D1*D1+D2*D2+D3*D3)
           COSA = (D1*ALFA+D2*BETA+D3*GAMMA)/DD
           IF(ABS (COSA).GT.1.) COSA = 1.
           ANG = ACOS (COSA)
           IF (ANG.LT.0.001) ANG=0.001
           SINA= SIN(ANG)
C
C               compute error on exit angle due to expt resolution
C               error depends also on cluster width !!
C
           FX=(ALFA-D1/DD*COSA)/(DD*SINA)
           FY=(BETA-D2/DD*COSA)/(DD*SINA)
           FZ=(GAMMA-D3/DD*COSA)/(DD*SINA)
           IF (NSUBC(1,N1).GT.1) THEN
C                                                 barrel+midangle Int
              XQ=XX(1,N1)*XX(1,N1)
              YQ=YY(1,N1)*YY(1,N1)
              RQ=XQ+YQ
              DTQ1=(FX*FX*YQ/RQ +FY*FY*XQ/RQ+FZ*FZ)*XRES2
           ELSE
C                                                endcap Int
              DTQ1=(FX*FX+FY*FY)*XRES2
           ENDIF
           IF (NSUBC(2,N2).GT.1) THEN
C                                                 barrel+midangle Ext
              XQ=XX(2,N2)*XX(2,N2)
              YQ=YY(2,N2)*YY(2,N2)
              RQ=XQ+YQ
              DTQ2=(FX*FX*YQ/RQ +FY*FY*XQ/RQ+FZ*FZ)*XRES2
           ELSE
C                                                endcap Ext
              DTQ2=(FX*FX+FY*FY)*XRES2
           ENDIF
           AAVG   = SQRT(PIBY2*(THSQ + DTQ1 + DTQ2 ))
           ACUT   = ACCANG*AAVG
C                          put a lower limit on ACUT to account for
C                          chamber misalignment. G. Capon sept 1991
           IF (ACUT.LT.0.085) ACUT=0.085
           IF (ANG.LT.ANGMI) THEN
              ANGMI=ANG
              AAVGM=AAVG
              ACUTM=ACUT
           ENDIF
 20        CONTINUE
C               fill histos
           R02=AAVGM*AAVGM/PIBY2
           CHI2=ANGMI*ANGMI/R02
           PCHI2=PROB(CHI2,2)
           CALL HFILL(8014,PCHI2,0.,1.)
           ANORM=ANGMI/AAVGM
           CALL HFILL(8006,ANORM,0.,1.)
           CALL HFILL(8016,ANGMI,0.,1.)
           CALL HFILL(8017,AAVGM,0.,1.)
C
C?       if exit angle > cut, the track is no more associated
C
           IF(ANGMI.GT.ACUTM) THEN
               IF(IMPRIN.GE.1)WRITE(LDEBRL,78) ANGMI,ACUT
               IASS=0
           ELSE
               IF(IMPRIN.GE.1)WRITE(LDEBRL,79) ANGMI,ACUT
               ITEXI=1
           ENDIF
      ENDIF
C
C?       fill MCAD bank
C?       the variables DISMI,DCUTM,ANGMI,ACUT are set to zero if the
C?       test is not passed or if hits are absent in one muon chamber
C?       layer (internal or external).
C
      JMCAD=IW(NAMCAD)
      KMCAD=KNEXT(JMCAD)
      IF(IASS.GT.0) THEN
           IW(KMCAD+JMCANH)= NASS(1)
           IW(KMCAD+JMCANH+1)= NASS(2)
           IF (NASS(1).GT.0) RW(KMCAD+JMCADH)= DISMI(1)
           IF (NASS(2).GT.0) RW(KMCAD+JMCADH+1)= DISMI(2)
           IF (NASS(1).GT.0) RW(KMCAD+JMCADC)= DCUTM(1)
           IF (NASS(2).GT.0) RW(KMCAD+JMCADC+1)= DCUTM(2)
           IF (ITEXI.GT.0) RW(KMCAD+JMCAAM)= ANGMI
           IF (ITEXI.GT.0) RW(KMCAD+JMCAAC)= ACUT
           IW(KMCAD+JMCATN)= ITRAC
           IW(JMCAD+2)=IW(JMCAD+2)+1
      ELSE
C                     reset MTHR pointer if no hits associated (bad exit angle)
           IW(JMTHR+2)=LRMEM
      ENDIF
C
C                      count tested and associated tracks for job stat
C
      NTRTST=NTRTST+1
      NTRASS=NTRASS+IASS
C
 99   RETURN
C
 76   FORMAT( I3,' muon hits associated to track',I3,' on layer 1, ',
     *'DISTmin =',F5.1,', DISTcut =',F5.1)
 77   FORMAT( I3,' muon hits associated to track',I3,' on layer 2, ',
     *'DISTmin =',F5.1,', DISTcut =',F5.1)
 78   FORMAT('  Angle test for track ass.to muchhits NOT passed, ANGMI='
     *,F7.3,',ACUT=',F7.3,' rad')
 79   FORMAT('  Angle test for track ass.to muon ch.hits passed, ANGMI='
     *,F7.3,',ACUT=',F7.3,' rad')
C
      END
#endif
