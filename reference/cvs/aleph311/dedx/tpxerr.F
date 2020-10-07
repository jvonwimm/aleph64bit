      FUNCTION TPXERR(NAME,XNS,TL,IER)
C-------------------------------------------------------------------
C! Get expected relative dE/dx error
CKEY DEDX TPC ERROR
C  R. Johnson  9-2-90
C  D. Casper  14-6-95  Create TPXERR from TDXERR
C
C  Input:   NAME    /A     'WIRE' or 'PAD'
C           XNS     /R     Number of dE/dx samples before truncation
C           TL      /R     Track length
C  Output:  TPXERR  /R     Error on the dE/dx (units 1=minion)
C           IER     /I     Non-zero if database banks not found
C
C  Input banks:
C     WIRE dE/dx - TC4X (calibration)
C     PAD  dE/dx - TP4X (calibration)
C     TC4X and TP4X have the same HAC parameters
C----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tc4xjj.h"
#include "tp4xjj.h"
C
      CHARACTER*(*) NAME
      CHARACTER*4 TNAM, CHAINT
      LOGICAL FOUND
C
C - set necessary data for GTDBBK
      INTEGER ALGTDB, GTSTUP
      CHARACTER DET*2, LIST*8
      PARAMETER (DET='TP', LIST='TC4XTP4X')
      DATA IROLD/0/
C
      DATA NTC4X, NTP4X /2*0/
      DATA WIRNT/0.4/
      DATA PADNT/3.0/
C
#include "bmacro.h"
C
      IF (XNS.LE.0.) THEN
        CALL ALTELL(' TPXERR: XNS=0. Abort to avoid divide by 0',0,
     &              'RETURN')
        TPXERR=.2
      ENDIF
C
      IF (NTC4X.EQ.0) THEN
        NTP4X=NAMIND('TP4X')
        NTC4X=NAMIND('TC4X')
      ENDIF
C
C++   Look for the calibration constants
#include "gtdbbk.h"
C
C - WIRE or PAD
C
      IF (NAME(1:1) .EQ. 'P') THEN
         NTNAM = NTP4X
         TNANT = PADNT
         JTNARP= JTP4RP
         JTNAMN= JTP4MN
         JTNAMX= JTP4MX
      ELSE
         NTNAM = NTC4X
         TNANT = WIRNT
         JTNARP= JTC4RP
         JTNAMN= JTC4MN
         JTNAMX= JTC4MX
      ENDIF
C
      KTNAM=IW(NTNAM)
      IF (KTNAM.EQ.0) THEN
          IER=4
          GO TO 999
      ENDIF
C
C++   Get data-base constants for resolution parameterization
C
      DX=TL/XNS
      SMPL=ALOG(DX/TNANT)
      DO 34 I=1,LROWS(KTNAM)
        IF (SMPL.GE.RTABL(KTNAM,I,JTNAMN)
     &         .AND. SMPL.LE.RTABL(KTNAM,I,JTNAMX)) THEN
          IB=I
          GO TO 35
        ENDIF
   34 CONTINUE
      TNAM = CHAINT (IW(KTNAM-3))
      CALL ALTELL(' TPXERR: sample length out of range of bank '//TNAM,
     &            0,'RETURN')
      IB=1
   35 CONTINUE
C
      RFLAG= RTABL(KTNAM,IB,JTNARP+6)
      IF (RFLAG.EQ.0. .AND. IRUN.GT.2000) THEN
C
C++     22 July 1991:
C++     Parameterization for calibration used on 89/90 data.
C++     This will no longer be used if 89/90 data are reprocessed.
C
        A= RTABL(KTNAM,IB,JTNARP+3)
        B= RTABL(KTNAM,IB,JTNARP+4)
        ARG= A/XNS + B
        IF (ARG.LT.0.) THEN
          TNAM = CHAINT (IW(KTNAM-3))
          CALL ALTELL(' TPXERR: SQRT of negative number. '//
     &         'Bad calibration consts in bank '//TNAM,0,'RETURN')
          TPXERR=0.055
        ELSE
          TPXERR=SQRT(ARG)
        ENDIF
      ELSE
C
C++     This is the prefered formula, which takes into account both
C++     the sample-length dependence and the number of wires
C
        PNORM= RTABL(KTNAM,IB,JTNARP)
        PPOW1= RTABL(KTNAM,IB,JTNARP+1)
        PPOW2= RTABL(KTNAM,IB,JTNARP+2)
C
        TPXERR= PNORM*(DX**PPOW2)*(XNS**PPOW1)
      ENDIF
C
      IER=0
  999 CONTINUE
      RETURN
      END
#endif
