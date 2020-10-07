      SUBROUTINE LTRACE
C-----------------------------------------------------------------------
C! Connect lumi tracks with clusters
C!
C!   Author   : P. H. Hansen   890101
C!   Modified : H. Meinhard    05-Mar-1990
C!   Input    : 'SFTR' 'LSDA' 'LCLU'
C!   Output   : 'LTRK'
C!   Created  : 'LTRK'
C!
C!   Description :
C!   ===========
C?     Create a temporary bank LTRK.
C?     Create electron candidate bank LIDT
C?     For each SFTR track
C?       find the tower and cluster being hit
C?       fill the CHarge column of LCLU
C?       fill LTRK row with
C?            2.  LCLU row
C?            3.  LSDA row
C?            4.  dist to cluster
C?          5-6.  dist to edges
C?
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "sanamc.h"
#include "lsdajj.h"
#include "lclujj.h"
#include "lobjjj.h"
#include "sftrjj.h"
#include "ltrkjj.h"
#include "lcrejj.h"
#include "lwbank.h"
#include "lmapjj.h"
#include "srecpc.h"
#include "alcons.h"
      DIMENSION DIST(4)
#include "bmacro.h"
C----------------------------------------------------------------------
      KSFTR = IW(NAMIND('SFTR'))
      IF(KSFTR.LE.0)                                    GOTO 999
      NTRK = LROWS(KSFTR)
      IF(NTRK.LE.0)                                     GOTO 999
C
C Create LTRK
      LEN = LMHLEN + LLTRKA*NTRK
      CALL AUBOS('LTRK',0,LEN,KLTRK,IGARB)
      IF(IGARB.EQ.2)                                    GOTO 997
      IW(KLTRK+1) = LLTRKA
      IW(KLTRK+2) = NTRK
C
      IF(IGARB.NE.0) KSFTR=IW(NAMIND('SFTR'))
      KLMAP = IW(NALMAP)
      KLSDA = IW(NALSDA)
      KLCLU = IW(NALCLU)
      KLOBJ = IW(NALOBJ)
      KLCRE = IW(NALCRE)
C
C For each SFTR track
      DO 200 ITRK = 1,NTRK
         ITOW = 0
         ICLU = 0
C
C Find the hit tower and distance to acceptance edge
         TH = RTABL(KSFTR,ITRK,JSFTTH)
         PH = RTABL(KSFTR,ITRK,JSFTPH)
         X0 = RTABL(KSFTR,ITRK,JSFTX0)
         Y0 = RTABL(KSFTR,ITRK,JSFTY0)
         IF (TH .LT. PIBY2) THEN
           PH = PH + RTABL(KLCRE,1,JLCRZC) * DPHBSR
           XT = X0 + RTABL(KLCRE,1,JLCRZC) * TAN(TH) * COS(PH)
           YT = Y0 + RTABL(KLCRE,1,JLCRZC) * TAN(TH) * SIN(PH)
           ZT = RTABL(KLCRE,1,JLCRZC)
         ELSE
           PH = PH - RTABL(KLCRE,1,JLCRZC) * DPHBSR
           XT = X0 - RTABL(KLCRE,1,JLCRZC) * TAN(TH) * COS(PH)
           YT = Y0 - RTABL(KLCRE,1,JLCRZC) * TAN(TH) * SIN(PH)
           ZT = -RTABL(KLCRE,1,JLCRZC)
         ENDIF
         THETA = ATAN(SQRT(XT**2+YT**2)/ZT)
         IF (THETA .LT. 0.) THETA = THETA + PI
         PHI = ATAN2(YT,XT)
         IF (PHI .LT. 0.) PHI = PHI + TWOPI
         CALL LDIRTW(THETA,PHI,IADDR,DIST)
C
C Find the corresponding LSDA and LCLU row
         IF(IADDR .GT. 0) THEN
            ITOW = IW(KLMAP+LMHLEN+IADDR)
            IF(ITOW.NE.0) THEN
               ICLU = ITABL(KLSDA,ITOW,JLSDLC)
               DCT  = 25.5
               IF(ICLU.NE.0) THEN
C
C Fill the charge in LOBJ
                  KCL = KROW(KLOBJ,ICLU)
                  IW(KCL+JLOBCH) = 1
C
C Find the track-to-cluster distance
                  THC = RTABL(KLOBJ,ICLU,JLOBTC)
                  PHC = RTABL(KLOBJ,ICLU,JLOBPC)
                  RAC = 280.
                  XC  = RAC*COS(PHC)*SIN(THC)
                  YC  = RAC*SIN(PHC)*SIN(THC)
                  XT  = RAC*COS(PHI)*SIN(THETA)
                  YT  = RAC*SIN(PHI)*SIN(THETA)
                  DCT = SQRT((XC-XT)**2+(YC-YT)**2)
               ENDIF
            ENDIF
         ENDIF
C
C Fill LTRK
         KL = KROW(KLTRK,ITRK)
         IW(KL + JLTRLC) = ICLU
         IW(KL + JLTRLS) = ITOW
         RW(KL + JLTRDC) = DCT
         CALL UCOPY(DIST(1),RW(KL+JLTRDA),2)
  200 CONTINUE
C
C Dump the LTRK bank on request
      IF(FDEBRF.AND.JDBDRF(JULLC).GE.2) CALL PRTABL('LTRK',0)
      GOTO 999
C
  997 CALL REPORT('LTRACE','AUBOS failure',1)
  999 CONTINUE
      END
#endif
