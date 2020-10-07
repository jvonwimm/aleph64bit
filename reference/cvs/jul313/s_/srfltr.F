      SUBROUTINE SRFLTR(NKAN)
C----------------------------------------------------------------------
C! Fill track cand. in track bank
C!
C!    Author:     H. Meinhard       05-Jun-1987
C!    Modified:   H. Meinhard       20-Jun-1988
C!
C!    Input:      - NKAN      /I    number of track candidate to
C!                                  be filled
C!
C!    Description
C!    ===========
C!    Convert cartesian parameters of track candidates to spherical
C!    parameters and fill it into track bank and track to coordinates
C!    pointer bank
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "sanamc.h"
#include "skanjj.h"
#include "skcpjj.h"
#include "stcpjj.h"
#include "strkjj.h"
#include "swrkbk.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C link to tracks and pointers
      KSTRK = IW(NASTRK)
      KSTCP = IW(NASTCP)
C
C get parameters of candidate
      ISIDE = ITABL(KWK1SW,NKAN,JSKASI)
      NP    = ITABL(KWK1SW,NKAN,JSKANP)
      C2    = RTABL(KWK1SW,NKAN,JSKAC2)
      AX    = RTABL(KWK1SW,NKAN,JSKAAX)
      AY    = RTABL(KWK1SW,NKAN,JSKAAY)
      XX    = RTABL(KWK1SW,NKAN,JSKAXX)
      XY    = RTABL(KWK1SW,NKAN,JSKAXY)
      YY    = RTABL(KWK1SW,NKAN,JSKAYY)
C
C perform conversion to spherical coordinates
      AX2   = AX*AX
      AY2   = AY*AY
      AXAY  = AX*AY
      THETA = ATAN(SQRT(AX2+AY2))
      PHI   = AMOD(ATAN2(AY,AX)+TWOPI,TWOPI)
      COVTT = (XX*AX2 + 2.*XY*AXAY + YY*AY2) / THETA**2
      COVTP = (-XX*AXAY + XY*(AX2-AY2) + YY*AXAY) / THETA**3
      COVPP = (XX*AY2 - 2.*XY*AXAY + YY*AX2) / THETA**4
C
C if track on side B, replace theta by complement to pi
      IF (ISIDE .EQ. 2) THETA = PI - THETA
C
C fill in track bank
      IW(KNEXT(KSTRK)+JSTRNP) = NP
      RW(KNEXT(KSTRK)+JSTRC2) = C2
      RW(KNEXT(KSTRK)+JSTRTH) = THETA
      RW(KNEXT(KSTRK)+JSTRPH) = PHI
      RW(KNEXT(KSTRK)+JSTRTT) = COVTT
      RW(KNEXT(KSTRK)+JSTRTP) = COVTP
      RW(KNEXT(KSTRK)+JSTRPP) = COVPP
      IW(KNEXT(KSTRK)+JSTRQF) = 1
      DO 300 II = 1, NP
  300 IW(KNEXT(KSTCP)+JSTCTC+II-1) = ITABL(KWK2SW,NKAN,JSKCKC+II-1)
      IW(KSTRK+LMHROW) = LROWS(KSTRK)+1
      IW(KSTCP+LMHROW) = LROWS(KSTCP)+1
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
