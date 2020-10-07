      SUBROUTINE OBSINR
COS   ==========================
COS   modified version of VBSINR, O.Schneider, Sept 1996
C----------------------------------------------------------------------
C!  - Initialise Beam Spot position computation for the run
C!
C!   Author   :- E. Lancon             29-APR-1993
C!   Modified :- E. Lancon             24-JUN-1994
C!        Create working bank only once
C!   Modified: Steve Wasserbaech, April 1995
C!   Always clear the work banks (number of rows = 0)
C!   Get ALLR from daf
C!   Modified: SW, 18 October 1995
C!   Switch to ALRP and VBPC; introduce VBSP card.
C?
C!======================================================================
#ifndef DOC
C     IMPLICIT NONE
      SAVE FIRST, WORK, NAALRP, NAVBPC, NAVBSP
      SAVE                              NAVBS2
#include "alpbjj.h"
#include "alrpjj.h"
#include "bcs.h"
#include "rcurnt.h"
#include "obscom.h"
C
      INTEGER IBUF, IGT, LEN, LUNDB
#ifdef ALP2
      INTEGER IGARB, KALPB
#endif
      INTEGER NAVBPC, KVBPC, NVBPC
      INTEGER NAVBSP, KVBSP
      INTEGER NAVBS2, KVBS2
      INTEGER NAALRP, KALRP, NALRP, IALRP
      INTEGER NLINK, INTCHA, NAMIND, GTSTUP, JUNIDB, MDARD
      LOGICAL FIRST, WORK
      LOGICAL ALLEP1
      DATA FIRST / .TRUE. /
      DATA WORK / .FALSE. /
C
#include "bmacrod.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C
C   Initialize a few things:
C
      IVBPC = -1
      ICHOP = 0
      NCHOP = 0
      XBMC = 0.
      YBMC = 0.
      SIGX0 = 1.
      SIGY0 = 1.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAALRP = NAMIND('ALRP')
        NAVBPC = NAMIND('VBPC')
        NAVBSP = NAMIND('VBSP')
        NAVBS2 = NAMIND('VBS2')
      ENDIF
      KVBS2 = IW(NAVBS2)
      IF(KVBS2.LE.0) NCHOP=1
C
C   Do nothing for MC unless VBSP card is present:
C
      KVBSP = IW(NAVBSP)
      IF (FMCRUN .AND. (KVBSP .LE. 0)) GO TO 999
C
C    Create buffering work banks:
C
      IF (.NOT. WORK) THEN
        WORK = .TRUE.
        DO IBUF=1,NBSBUF
          IBSTRK(IBUF) = 0
        ENDDO
        DO IBUF=1,NBSBUF
          CALL WBANK(IW, IBSTRK(IBUF), LMHLEN+500*LBPTRK, *820)
          IW(IBSTRK(IBUF)+LMHCOL) = LBPTRK
          IW(IBSTRK(IBUF)+LMHROW) = 0
          IW(IBSTRK(IBUF)-3) = INTCHA('WAPB')
        ENDDO
      ENDIF
C
C    Clear the work banks:
C
      DO IBUF=1,NBSBUF
        IW(IBSTRK(IBUF)+LMHROW) = 0
      ENDDO
C
C    Get constants:
C
      LUNDB = JUNIDB(0)
      IGT = MDARD(IW,LUNDB,'VBPC',0)
      IF (IGT .LE. 0) GO TO 800
      KVBPC = IW(NAVBPC)
      IF (KVBPC .LE. 0) GO TO 800
      NVBPC = LROWS(KVBPC)
      IGT = MDARD(IW,LUNDB,'ALRP',0)
      IF (IGT .LE. 0) GO TO 830
      KALRP = IW(NAALRP)
      IF (KALRP .LE. 0) GO TO 830
      NALRP = LROWS(KALRP)
C
C  Get the row number in VBPC = LEP energy regime number:
C
      IF (ALLEP1(IRUNRC)) THEN
        IVBPC = 1
      ELSE
        IVBPC = 2
      ENDIF
      IF (IVBPC .GT. NVBPC) GO TO 840
C
C  Get the row number in ALRP = BE setup code:
C
      IALRP = GTSTUP('BE',IRUNRC)
      IF ((IALRP .LT. 1) .OR. (IALRP .GT. NALRP)) GO TO 850
C
C  Get the nominal beam size:
C
      SIGX0 = RTABL(KALRP,IALRP,JALRXS)
      SIGY0 = RTABL(KALRP,IALRP,JALRYS)
C
C    Create ALPB bank for this run:
C
      LEN = LALPBA + LMHLEN
#ifdef ALP2
      CALL BDROP(IW,'ALP2')
      CALL AUBOS('ALP2',IRUNRC,LEN,KALPB,IGARB)
      IF (IGARB .EQ. 2) GO TO 810
#else
      CALL WDROP(IW,KALPB)
      CALL WBANK(IW,KALPB,LEN,*810)
      IW(KALPB-3)=INTCHA('ALP2')
#endif
      IW(KALPB+LMHCOL) = LALPBA
      IW(KALPB+LMHROW) = 1
      IW(KROW(KALPB,1)+JALPFE) = 1
C
      IBSFEV = 1
      IBSBUF = 1
      JBSBUF = 2
C
C   If a fixed number of chunks are desired (for special studies),
C   the number may be given as a token on the VBSP card.  If no token
C   is given, we make one big chunk.  This feature is not used in
C   normal julia operations.  (NCHOP = 0 means normal mode.)
C
      IF (KVBSP .GT. 0) THEN
        NCHOP = 1
        IF (IW(KVBSP) .GT. 0) THEN
          NCHOP = IW(KVBSP+1)
          IF (NCHOP .LT. 1) GO TO 860
        ENDIF
      ENDIF
C
      GO TO 999
C
C   Errors
C
  800 CALL RERROR ('OBSINR', -1, 'VBPC bank not found on daf!')
      GO TO 999
C
#ifdef ALP2
  810 CALL RERROR ('OBSINR', -2, 'No space to book ALP2!')
#else
  810 CALL RERROR ('OBSINR', -2, 'No space to book ALP2 work bank!')
#endif
      GO TO 999
C
  820 CALL RERROR ('OBSINR', -3, 'No space to book work bank!')
      GO TO 999
C
  830 CALL RERROR ('OBSINR', -4, 'ALRP bank not found on daf!')
      GO TO 999
C
  840 CALL RERROR ('OBSINR', -5, 'VBPC row number out of range!')
      GO TO 999
C
  850 CALL RERROR ('OBSINR', -6, 'ALRP row number out of range!')
      GO TO 999
C
  860 CALL RERROR ('OBSINR', -7, 'Invalid token on VBSP card!')
      GO TO 999
C
  999 RETURN
      END
#endif
