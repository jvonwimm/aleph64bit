      SUBROUTINE LTRACK
C-----------------------------------------------------------------------
C! Connect lumi tracks with clusters
C!
C!   Author   : P. H. Hansen   891215
C!   Modified : H. Meinhard    05-Mar-1990
C!   Input    : 'SFTR' 'LOBJ' 'LIDT'
C!   Output   : 'LIDT'
C!
C!   Description :
C!   ===========
C?     Connect lumi tracks with clusters (create and fill LTRK)
C?     For each cluster
C?       find the closest track
C?       store LIDT <--> SFTR relation
C?     Create and fill LIFL
C?     Do Bhabha analysis
C?
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "sanamc.h"
#include "srecpc.h"
#include "lsdajj.h"
#include "sftrjj.h"
#include "ltrkjj.h"
#include "lidtjj.h"
#include "lobjjj.h"
#include "lcrejj.h"
#include "lwbank.h"
#include "lmapjj.h"
#include "alcons.h"
C
      DIMENSION DIST(4)
      EXTERNAL NDROP,VMOD
#include "bmacro.h"
C----------------------------------------------------------------------
      KLIDT = IW(NALIDT)
      IF(KLIDT.LE.0)                                       GOTO 999
      NIDT = LROWS(KLIDT)
      IF(NIDT.LE.0)                                        GOTO 999
C
C Connect Lumi tracks with clusters
      KSFTR = IW(NAMIND('SFTR'))
      IF(KSFTR.LE.0)                                       GOTO 998
      CALL LTRACE
C
C Find closest track
      KLTRK = IW(NALTRK)
      KLOBJ = IW(NALOBJ)
      NTRK = 0
      IF(KSFTR.GT.0) NTRK = LROWS(KSFTR)
      IF(NTRK.GT.0) THEN
        DO 10 I=1,NIDT
          DMIN = 25.5
          JTRK = 0
          NTR = 0
          ICLU = ITABL(KLIDT,I,JLIDLO)
          IF(ITABL(KLOBJ,ICLU,JLOBCH).EQ.1) THEN
            DO 320 ITRK=1,NTRK
              IF(ITABL(KLTRK,ITRK,JLTRLC).NE.ICLU) GOTO 320
              KSFT = KROW(KSFTR,ITRK)
C
C Store relation to LIDT in SFTR
              IW(KSFT + JSFTLI) = NIDT
              NTR = NTR+1
              DIS = RTABL(KLTRK,ITRK,JLTRDC)
              IF(DIS.LT.DMIN) THEN
                DMIN = DIS
                JTRK = ITRK
                DIST(3) = RTABL(KLTRK,ITRK,JLTRDA)
                DIST(4) = RTABL(KLTRK,ITRK,JLTRDA+1)
              ENDIF
  320       CONTINUE
          ENDIF
C
C Store the relation to SFTR in LIDT
          KID = KROW(KLIDT,I)
          IW(KID+JLIDNT) = NTR
          IW(KID+JLIDST) = JTRK
          RW(KID+JLIDDA+2) = DIST(3)
          RW(KID+JLIDDA+3) = DIST(4)
   10   CONTINUE
      ENDIF
C
C Drop track - cluster relation
      KLTRK = NDROP('LTRK',0)
  998 CONTINUE
C
C Fill acceptance flags for all electron candidates
      CALL LIDCOD
C
C Debug
      IF(FDEBRF.AND.JDBDRF(JULLC).GE.2) CALL PRTABL('LIDT',0)
      IF(FDEBRF.AND.JDBDRF(JULLC).GE.2) CALL PRTABL('LIFL',0)
C
  999 CONTINUE
C
C Do luminosity analysis
      CALL LBHAB
      END
#endif
