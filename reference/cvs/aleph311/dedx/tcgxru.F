      INTEGER FUNCTION TCGXRU (IRUN,IROW)
C --------------------------------------------------------------
C!  get a row of bank TCGXRU (after the model of TDFVRU)
C - W.Wiedenmann-930831
CKEY DEDX TPC CALIBRATION
C
C - Input:
C             IRUN   / INTE  = run number
C
C - Output:   TCGXRU / INTE  = TCGX BOS index
C                              =0 means not enough space
C                              <0 means a garbage collection occurded
C             IROW   / INTE  = row # for the known IRUN
C                              0 means IRUN not found
C
C ----------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*2 DIR
      INTEGER GTDBAS
#include "bcs.h"
#include "tcgxjj.h"
      DATA NTCGX /0/
      DATA IPRUN /0/
#include "bmacro.h"
C ----------------------------------------------------------------
C
C - 1st entry
C
      IF (NTCGX.EQ.0) THEN
         NTCGX = NAMIND('TCGX')
         NCDAF = NAMIND('CDAF')
         LDBAS = JUNIDB(0)
         JTCGX = IW(NTCGX)
         IF (JTCGX.NE.0) THEN
C      NRUN is the bank number of the current TCGX bank
            NRUN = IW(JTCGX-2)
C      LAST is the highest element in the  TCGX,NR=NRUN bank
            LAST = ITABL(JTCGX,LROWS(JTCGX),JTCGLR)
         ENDIF
      ENDIF
C
C - get TCGX bank
C
      IGARB = 0
      NR = IRUN
      DIR = 'LE'
      JTCGX = IW(NTCGX)
 40   IF (JTCGX.EQ.0) THEN
C     get TCGX from the data base if any
C
C     1st  check validity range of the data base for real data
         IF (IW(NCDAF).EQ.0.AND.IPRUN.NE.IRUN.AND.IRUN.GE.2001) THEN
            IPRUN = IRUN
            IGET = GTDBAS (LDBAS,IRUN)
            IF (IGET.NE.0) THEN
               TCGXRU = 0
               RETURN
            ENDIF
         ENDIF
C
         NRUN = NDANR (LDBAS,'TCGX',DIR,NR)
         IF (NRUN.NE.0) THEN
            JTCGX = MDARD (IW,LDBAS,'TCGX',NRUN)
            IF (JTCGX.EQ.0) THEN
               IGARB=1
               CALL BGARB(IW)
               JTCGX = MDARD (IW,LDBAS,'TCGX',NRUN)
               IF (JTCGX.EQ.0) GOTO 60
            ENDIF
C           LAST is the highest element in the d.b NAME,NR=NRUN bank
            LAST = ITABL(JTCGX,LROWS(JTCGX),JTCGLR)
         ENDIF
      ENDIF
C
C - get the row # IROW  which contains the run # NR
C
      IF (JTCGX.GT.0) THEN
 50      LC = LCOLS(JTCGX)
         LR = LROWS(JTCGX)
C
C     IF the run # IRUN is greater than the last run THEN
C        IF a TCGX bank with a higher bank # exists THEN
C           use this TCGX bank
C        ELSE
C           look at the data base with a IRUN greater than the LAST one
C        ENDIF
C     ELSEIF IRUN is smaller than the 1st one THEN
C        look at the data base
C     ELSE
C        find the right row # in TCGX bank
C     ENDIF
C
         IF (IRUN .GT. ITABL(JTCGX,LR,JTCGLR)) THEN
            IF (IW(JTCGX-1) .GT. 0) THEN
               JTCGX = IW(JTCGX-1)
               GOTO 50
            ELSE
               NR = LAST+1
               DIR = 'GE'
               JTCGX = 0
               GOTO 40
            ENDIF
         ELSEIF (IRUN .LT. NRUN) THEN
            DIR = 'LE'
            JTCGX = 0
            GOTO 40
         ELSE
C
            IROW = LOCTAB (IW(JTCGX+LMHLEN+1),LC,LR,JTCGFR,IRUN)
            IF (IROW.EQ.0) THEN
C           IRUN is outside run range
               JTCGX = 0
            ELSEIF (IROW.LT.0) THEN
C           IRUN is between 1st run of row # IROW and 1st run of
C           row # IROW+1
C           check that it is in the run range of row # IROW
               IROW = -IROW
               IF (IRUN.GT.ITABL(JTCGX,IROW,JTCGLR)) JTCGX = 0
            ENDIF
         ENDIF
      ENDIF
C
C - end
C
 60   CONTINUE
      TCGXRU = JTCGX
      IF (IGARB.EQ.1) TCGXRU = -JTCGX
      END
#endif
