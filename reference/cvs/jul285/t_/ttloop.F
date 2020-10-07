      INTEGER FUNCTION TTLOOP(ITK)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- D.Cinabro              3-DEC-1990
C!   Modified :- D.Cinabro             19-MAY-1991
C!
C!   Inputs:ITK /I FRFT row number of track under consideration
C!        -
C!
C!   Outputs: 0 if FRFT paramters are the only loop of the track
C!        -   >0 row in TARC of the last loop on the track
C!            <0 if an error has occured
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   Find which row in TCAL this track points to.  If TCAL indicates
C!   there is more than one arc, loop in TARC and find which is the
C!   final arc and return
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "tgftjj.h"
#include "t1ftjj.h"
#include "tcaljj.h"
#include "tarcjj.h"
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NTGFT = NAMIND('TGFT')
        NT1FT = NAMIND('T1FT')
        NTCAL = NAMIND('TCAL')
        NTARC = NAMIND('TARC')
      ENDIF
C
      TTLOOP = 0
C
C No TGFT or T1FT banks, hopeless to look for loops
C
      KTGFT = IW(NTGFT)
      KT1FT = IW(NT1FT)
      IF (KTGFT.LE.0.OR.KT1FT.LE.0) THEN
        GOTO 999
      ENDIF
C
C Find the track in the TGFT bank
C
      DO 100 ITGF = 1,LROWS(KTGFT)
  100   IF (ITK.EQ.ITABL(KTGFT,ITGF,JTGFTC)) GOTO 101
C
C No TGFT track found.  Cannot find loops.
C
      GOTO 999
C
  101 CONTINUE
C
C Find the tracks in T1FT
C
      KTCAL = IW(NTCAL)
      IF (KTCAL.LE.0) THEN
        TTLOOP = -2
        GOTO 999
      ENDIF
      ILOP = 0
      DO 200 IT1 = 1,LROWS(KT1FT)
        IF (ITGF.EQ.ITABL(KT1FT,IT1,JT1FTG)) THEN
C
C Pointer into TCAL
C
          ITCL = ITABL(KT1FT,IT1,JTGFTC)
C
C Is there more than 1 arc?
C
          NFOL = ITABL(KTCAL,ITCL,JTCANR)
          IF (NFOL.GT.0) THEN
C
C So there are loops.  Find the last one in TARC.
C
            ILOP = 0
            IBIG = 0
            KTARC = IW(NTARC)
            IF (KTARC.LE.0) THEN
              TTLOOP = -3
              GOTO 999
            ENDIF
            DO 10 IARC = 1,LROWS(KTARC)
              ITN = ITABL(KTARC,IARC,JTARTN)
              IF (ITN.EQ.ITCL) THEN
                NARC = ABS(ITABL(KTARC,IARC,JTARAN))
                IF (NARC.GT.IBIG) THEN
                  IBIG = NARC
                  ILOP = IARC
                ENDIF
              ENDIF
   10       CONTINUE
C
C Only one of the TCAL's should have arcs
C
            GOTO 201
          ENDIF
        ENDIF
  200 CONTINUE
  201 CONTINUE
C
C That is it.  ILOP should have the last arc
C
      TTLOOP = ILOP
C
  999 RETURN
      END
#endif
