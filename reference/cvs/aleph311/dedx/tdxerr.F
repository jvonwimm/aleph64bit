      FUNCTION TDXERR(NS,TL,IER)
C
C-------------------------------------------------------------------
C! Get expected relative dE/dx error
CKEY DEDX TPC ERROR
C  R. Johnson  9-2-90
C
C  Input:   NS      /I     Number of dE/dx samples before truncation
C           TL      /R     Track length
C  Output:  TDXERR  /R     Error on the dE/dx (units 1=minion)
C           IER     /I     Non-zero if database banks not found
C
C----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tc4xjj.h"
C
      LOGICAL FOUND
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*4
      PARAMETER (DET='TP', LIST='TC4X')
      DATA IROLD/0/
C
      DATA NTC4X/0/
      DATA WIRNT/0.4/
C
#include "bmacro.h"
C
      IF (NS.LE.0) THEN
        CALL ALTELL(' TDXERR: NS=0.  Abort to avoid divide by 0',0,
     &              'RETURN')
        TDXERR=.2
      ENDIF
      IF (NTC4X.EQ.0) THEN
        NTC4X=NAMIND('TC4X')
      ENDIF
C
C++   Look for the calibration constants
#include "gtdbbk.h"
      KTC4X=IW(NTC4X)
      IF (KTC4X.EQ.0) THEN
          IER=4
          GO TO 999
      ENDIF
C
C++   Get data-base constants for resolution parameterization
C
      DX=TL/FLOAT(NS)
      SMPL=ALOG(DX/WIRNT)
      DO 34 I=1,LROWS(KTC4X)
        IF (SMPL.GE.RTABL(KTC4X,I,JTC4MN)
     &         .AND. SMPL.LE.RTABL(KTC4X,I,JTC4MX)) THEN
          IB=I
          GO TO 35
        ENDIF
   34 CONTINUE
      CALL ALTELL(' TDXERR: sample length out of range of bank TC4X.',0,
     &            'RETURN')
      IB=1
   35 CONTINUE
C
      RFLAG= RTABL(KTC4X,IB,JTC4RP+6)
      IF (RFLAG.EQ.0. .AND. IRUN.GT.2000) THEN
C
C++     22 July 1991:
C++     Parameterization for calibration used on 89/90 data.
C++     This will no longer be used if 89/90 data are reprocessed.
C
        A= RTABL(KTC4X,IB,JTC4RP+3)
        B= RTABL(KTC4X,IB,JTC4RP+4)
        ARG= A/FLOAT(NS) + B
        IF (ARG.LT.0.) THEN
          CALL ALTELL(' TDXERR: SQRT of negative number. '//
     &             'Bad calibration consts in bank TC4X?',0,'RETURN')
          TDXERR=0.055
        ELSE
          TDXERR=SQRT(ARG)
        ENDIF
      ELSE
C
C++     This is the prefered formula, which takes into account both
C++     the sample-length dependence and the number of wires
C
        PNORM= RTABL(KTC4X,IB,JTC4RP)
        PPOW1= RTABL(KTC4X,IB,JTC4RP+1)
        PPOW2= RTABL(KTC4X,IB,JTC4RP+2)
C
        TDXERR= PNORM*(DX**PPOW2)*((FLOAT(NS))**PPOW1)
      ENDIF
C
      IER=0
  999 CONTINUE
      RETURN
      END
#endif
