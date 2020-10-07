      SUBROUTINE LSELBA(IA,IB)
C-----------------------------------------------------------------------
C! Selection of Bhabha background
C!
C!   Author   : P. H. Hansen  890701
C! Modified   : J. Wear       891119
C! Modified   : P. Comas      940502
C! Modified   : P. H. Hansen  950401
C! Modified   : P. H. Hansen  960501
C!   Ignore scalefactors. Drop cut on summed energy (to gain stat)
C!
C!     Input  :  IA,IB           row numbers in LSIN
C!               'LSIN'          single arm storage bank
C!               'BHAB'          Bhabha selection criteria
C!     Output :  'LBAK'          Fake Bhabha event statistics
C!   Description :
C!   ===========
C?      Decide which side has the smaller fiducial area.
C?      For each requested measurement method require successively:
C?         At least one cluster over high threshold.
C?         Both clusters over low threshold.
C?         Both clusters be contained in their proper fiducial areas.
C?         Coplanarity.
C?      Update a counter in LBAK after each passed cut.
C?
C-----------------------------------------------------------------------
C
#ifndef DOC
#include "rconds.h"
#include "bcs.h"
#include "rflags.h"
#include "lcnamc.h"
#include "lsinjj.h"
#include "lbakjj.h"
#include "bhabjj.h"
#include "alcons.h"
#include "rcurnt.h"
#include "xtcnjj.h"
#include "x1rgjj.h"
#include "x1ttjj.h"
      COMMON/DWNSCL/ SCALE(2)
      PARAMETER (NLBIT=5)
      DIMENSION LLBIT(NLBIT),LHBIT(NLBIT),LWTLO(NLBIT),LWTHI(NLBIT)
      INTEGER HIBIT
      EXTERNAL RNDM
      CHARACTER*8 TLOSG,THISG
      LOGICAL FSATR,FTRAC,BTEST
      DATA TLOSG,THISG /'LO_SING','HI_SING'/
      DATA LRUN /-9999/
#include "bmacro.h"
C---------------------------------------------------------------------
      IF(IA.LE.0.OR.IB.LE.0)                        GOTO 999
C
C Banks needed
      KBHAB = IW(NABHAB)
      IF(KBHAB.LE.0)                                GOTO 999
      NMETH = LROWS(KBHAB)
      IF(NMETH.LE.0)                                GOTO 999
      KLBAK = IW(NALBAK)
      IF(KLBAK.LE.0)                                GOTO 999
      KLSIN = IW(NALSIN)
      IF(KLSIN.LE.0)                                GOTO 999
C
C Take beam energy
      EBEAM = ENERRC/2.
C
C Real Data event weight for background.
        W = 1.
C
C type of background trigger:
C  Low threshold single
        IT = 0
C  Very high threshold single
        IF(ITABL(KLSIN,IA,JLSIIT).EQ.3.OR.
     &     ITABL(KLSIN,IB,JLSIIT).EQ.3) IT=1
C
C Monte Carlo event weight
      KKEVH = IW(NAMIND('KEVH'))
      IF(KKEVH.GT.0) W = RTABL(KKEVH,1,5)
C
C Energy distribution
      ENA = RTABL(KLSIN,IA,JLSIEN)
      ENB = RTABL(KLSIN,IB,JLSIEN)
C
C Acoplanarity
      PHA = RTABL(KLSIN,IA,JLSIPH)
      PHB = RTABL(KLSIN,IB,JLSIPH)
      DPHI = ABS(PHA-PHB)
      DPHI = AMIN1(DPHI,TWOPI-DPHI)
C
C Define small and large fiducial areas
      K1 = ITABL(KLSIN,IA,JLSIK1)
      K2 = ITABL(KLSIN,IB,JLSIK2)
      KE1 = ITABL(KLSIN,IA,JLSIKH)
      KE2 = ITABL(KLSIN,IB,JLSIKL)
      IF1 = ITABL(KLSIN,IA,JLSIIF)
      IF2 = ITABL(KLSIN,IB,JLSIIF)
      IF(RNDM(IDUM).GT.0.5) THEN
        K1 = ITABL(KLSIN,IB,JLSIK1)
        K2 = ITABL(KLSIN,IA,JLSIK2)
        KE1 = ITABL(KLSIN,IB,JLSIKH)
        KE2 = ITABL(KLSIN,IA,JLSIKL)
        IF1 = ITABL(KLSIN,IB,JLSIIF)
        IF2 = ITABL(KLSIN,IA,JLSIIF)
      ENDIF
C
C SATR flag
      FSATR = .FALSE.
      IF(BTEST(IF1,2).AND.BTEST(IF2,2)) FSATR = .TRUE.
C
C Loop over methods
      IAA=IA
      IBB=IB
      DO 100 IM=1,NMETH
        KBAK = KROW(KLBAK,IM)
        IB = IM-1
C
C Method type
        MET = ITABL(KBHAB,IM,JBHAME)
C
C Required track quality
        IQA = ITABL(KBHAB,IM,JBHAQF)
C
C Track flag
        FTRAC = .FALSE.
        IF(BTEST(IF1,IQA).AND.BTEST(IF2,IQA)) FTRAC = .TRUE.
        IF(MET.EQ.2.AND.BTEST(IF1,IQA))       FTRAC = .TRUE.
C
C Apply energy thresholds - BUT NOT ON ENERGY SUM!
        IF(.NOT.BTEST(KE1,IB))                        GOTO 100
        IF(.NOT.BTEST(KE2,IB))                        GOTO 100
        RW(KBAK+JLBAHI+IT) = RW(KBAK+JLBAHI+IT) + W
        RW(KBAK+JLBALO+IT) = RW(KBAK+JLBALO+IT) + W
C
C coplanarity
        IF(DPHI.GT.RTABL(KBHAB,IM,JBHADP))
     &    RW(KBAK+JLBACP+IT) = RW(KBAK+JLBACP+IT) + W
C
C Apply acoplanarity as last cut
        IF(MET.GE.4.AND.BTEST(K1,IB).AND.BTEST(K2,IB)) THEN
          IF(DPHI.LT.PI-RTABL(KBHAB,IM,JBHADP)) THEN
            RW(KBAK+JLBANP+IT) = RW(KBAK+JLBANP+IT) + W
            GOTO 100
          ELSEIF(DPHI.GT.RTABL(KBHAB,IM,JBHADP)) THEN
            RW(KBAK+JLBABH+IT) = RW(KBAK+JLBABH+IT) + W
            GOTO 100
          ENDIF
        ENDIF
        IF(MET.EQ.1.AND.BTEST(K1,IB).AND.BTEST(K2,IB)) THEN
          IF(DPHI.LT.PI-RTABL(KBHAB,IM,JBHADP)) THEN
            RW(KBAK+JLBANP+IT) = RW(KBAK+JLBANP+IT) + W
            GOTO 100
          ELSEIF(DPHI.GT.RTABL(KBHAB,IM,JBHADP)) THEN
            RW(KBAK+JLBABH+IT) = RW(KBAK+JLBABH+IT) + W
          ENDIF
        ENDIF
C
C Count accepted events in SATR and no SATR regions seperately
        IF(FSATR) THEN
          RW(KBAK+JLBASA+IT) = RW(KBAK+JLBASA+IT) + W
          IF(FTRAC) THEN
            RW(KBAK+JLBATR+IT) = RW(KBAK+JLBATR+IT) + W
            IF(.NOT.BTEST(K1,IB))                   GOTO 100
            RW(KBAK+JLBAF1+IT) = RW(KBAK+JLBAF1+IT) + W
            IF(MET.EQ.2) THEN
              IF(DPHI.LT.PI-RTABL(KBHAB,IM,JBHADP)) THEN
                RW(KBAK+JLBANP+IT) = RW(KBAK+JLBANP+IT) + W
                GOTO 100
              ENDIF
              IF(DPHI.GT.RTABL(KBHAB,IM,JBHADP)) THEN
                RW(KBAK+JLBABH+IT) = RW(KBAK+JLBABH+IT) + W
                GOTO 100
              ENDIF
            ENDIF
            IF(.NOT.BTEST(K2,IB))                   GOTO 100
            RW(KBAK+JLBAF2+IT) = RW(KBAK+JLBAF2+IT) + W
            IF(MET.EQ.3) THEN
              IF(DPHI.LT.PI-RTABL(KBHAB,IM,JBHADP)) THEN
                RW(KBAK+JLBANP+IT) = RW(KBAK+JLBANP+IT) + W
                GOTO 100
              ENDIF
              IF(DPHI.GT.RTABL(KBHAB,IM,JBHADP)) THEN
                RW(KBAK+JLBABH+IT) = RW(KBAK+JLBABH+IT) + W
              ENDIF
            ENDIF
          ENDIF
        ELSE
          RW(KBAK+JLBANS+IT) = RW(KBAK+JLBANS+IT) + W
          IF(.NOT.BTEST(K1,IB))                      GOTO 100
          RW(KBAK+JLBAF3+IT) = RW(KBAK+JLBAF3+IT) + W
          IF(.NOT.BTEST(K2,IB))                      GOTO 100
          RW(KBAK+JLBAF4+IT) = RW(KBAK+JLBAF4+IT) + W
        ENDIF
  100 CONTINUE
C
  999 CONTINUE
      END
#endif
