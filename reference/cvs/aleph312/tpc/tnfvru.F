      INTEGER FUNCTION TNFVRU (IRUN,IROW)
C --------------------------------------------------------------
C!  get a row of a bank
C - F.Ranjard - 900517
C
C - Modified by D. Casper 06-03-97 from TDFVRU to handle TNFV bank
CKEY TPC  GET BANK DA
C
C - Input:
C             IRUN   / INTE  = run number
C
C - Output:   TNFVRU / INTE  = TNFV BOS index
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
#include "tnfvjj.h"
      DATA NTNFV /0/ , IPRUN/0/
#include "bmacro.h"
C ----------------------------------------------------------------
C
C - 1st entry
C
      IF (NTNFV.EQ.0) THEN
         NTNFV = NAMIND('TNFV')
         NCDAF = NAMIND('CDAF')
         LDBAS = JUNIDB(0)
         JTNFV = IW(NTNFV)
         IF (JTNFV.NE.0) THEN
C      NRUN is the bank number of the current TNFV bank
            NRUN = IW(JTNFV-2)
C      LAST is the highest element in the  TNFV,NR=NRUN bank
            LAST = ITABL(JTNFV,LROWS(JTNFV),JTNFLR)
         ENDIF
      ENDIF
C
C - get TNFV bank
C
      IGARB = 0
      NR = IRUN
      DIR = 'LE'
      JTNFV = IW(NTNFV)
 40   IF (JTNFV.EQ.0) THEN
C     get TNFV from the data base if any
C
C     1st  check validity range of the data base for real data
         IF (IW(NCDAF).EQ.0.AND.IPRUN.NE.IRUN.AND.IRUN.GE.2001) THEN
            IPRUN = IRUN
            IGET = GTDBAS (LDBAS,IRUN)
            IF (IGET.NE.0) THEN
               TNFVRU = 0
               RETURN
            ENDIF
         ENDIF
C
         NRUN = NDANR (LDBAS,'TNFV',DIR,NR)
         IF (NRUN.NE.0) THEN
            JTNFV = MDARD (IW,LDBAS,'TNFV',NRUN)
            IF (JTNFV.EQ.0) THEN
               IGARB=1
               CALL BGARB(IW)
               JTNFV = MDARD (IW,LDBAS,'TNFV',NRUN)
               IF (JTNFV.EQ.0) GOTO 60
            ENDIF
C           LAST is the highest element in the d.b NAME,NR=NRUN bank
            LAST = ITABL(JTNFV,LROWS(JTNFV),JTNFLR)
         ENDIF
      ENDIF
C
C - get the row # IROW  which contains the run # NR
C
      IF (JTNFV.GT.0) THEN
 50      LC = LCOLS(JTNFV)
         LR = LROWS(JTNFV)
C
C     IF the run # IRUN is greater than the last run THEN
C        IF a TNFV bank with a higher bank # exists THEN
C           use this TNFV bank
C        ELSE
C           look at the data base with a IRUN greater than the LAST one
C        ENDIF
C     ELSEIF IRUN is smaller than the 1st one THEN
C        look at the data base
C     ELSE
C        find the right row # in TNFV bank
C     ENDIF
C
         IF (IRUN .GT. ITABL(JTNFV,LR,JTNFLR)) THEN
            IF (IW(JTNFV-1) .GT. 0) THEN
               JTNFV = IW(JTNFV-1)
               GOTO 50
            ELSE
               NR = LAST+1
               DIR = 'GE'
               JTNFV = 0
               GOTO 40
            ENDIF
         ELSEIF (IRUN .LT. NRUN) THEN
            DIR = 'LE'
            JTNFV = 0
            GOTO 40
         ELSE
C
            IROW = LOCTAB (IW(JTNFV+LMHLEN+1),LC,LR,JTNFFR,IRUN)
            IF (IROW.EQ.0) THEN
C           IRUN is outside run range
               JTNFV = 0
            ELSEIF (IROW.LT.0) THEN
C           IRUN is between 1st run of row # IROW and 1st run of
C           row # IROW+1
C           check that it is in the run range of row # IROW
               IROW = -IROW
               IF (IRUN.GT.ITABL(JTNFV,IROW,JTNFLR)) JTNFV = 0
            ENDIF
         ENDIF
      ENDIF
C
C - end
C
 60   CONTINUE
      TNFVRU = JTNFV
      IF (IGARB.EQ.1) TNFVRU = -JTNFV
      END
#endif
