      INTEGER FUNCTION LFILRU (IRUN,IROW)
C --------------------------------------------------------------
C!  get a row of a bank
C - F.Ranjard - 900517
CKEY ALEF GET BANK DA
C
C - Input:
C             IRUN   / INTE  = run number
C
C - Output:   LFILRU / INTE  = LFIL BOS index
C                              =0 means not enough space
C                              <0 means a garbage collection occurded
C             IROW   / INTE  = row # for the known IRUN
C                              0 means IRUN not found
C
C ----------------------------------------------------------------
#ifndef DOC
      CHARACTER*2 DIR
      INTEGER GTDBAS
      LOGICAL GREATER,SMALLER,FDBASE
#include "bcs.h"
#include "lfiljj.h"
      SAVE NCDAF, LDBAS, IPRUN, NRUN, LAST
      DATA NLFIL /0/
#include "bmacro.h"
C ----------------------------------------------------------------
C
C - 1st entry
C
      IF (NLFIL.EQ.0) THEN
         NLFIL = NAMIND('LFIL')
         NCDAF = NAMIND('CDAF')
         LDBAS = JUNIDB(0)
         JCOL  = JLFIFR
         IPRUN = -1
         JLFIL = IW(NLFIL)
         IF (JLFIL.NE.0) THEN
C      NRUN is the bank number of the current LFIL bank
            NRUN = IW(JLFIL-2)
C      LAST is the highest element in the  LFIL,NR=NRUN bank
            LAST = ITABL(JLFIL,LROWS(JLFIL),JLFILR)
         ENDIF
      ENDIF
C
C - get LFIL bank
C
      IGARB = 0
      NR = IRUN
      DIR = 'LE'
      GREATER = .FALSE.
      SMALLER = .FALSE.
      FDBASE  = .FALSE.
      JLFIL = IW(NLFIL)
 40   IF (JLFIL.EQ.0) THEN
C     get LFIL from the data base if any
C
C     1st  check validity range of the data base for real data
         IF (IW(NCDAF).EQ.0.AND.IPRUN.NE.IRUN.AND.IRUN.GE.2001) THEN
            IPRUN = IRUN
            IGET = GTDBAS (LDBAS,IRUN)
            IF (IGET.NE.0) THEN
               LFILRU = 0
               RETURN
            ENDIF
         ENDIF
C
         NRUN = NDANR (LDBAS,'LFIL',DIR,NR)
         IF (NRUN.NE.0) THEN
            JLFIL = MDARD (IW,LDBAS,'LFIL',NRUN)
            IF (JLFIL.EQ.0) THEN
               IGARB=1
               CALL BGARB(IW)
               JLFIL = MDARD (IW,LDBAS,'LFIL',NRUN)
               IF (JLFIL.EQ.0) GOTO 60
            ENDIF
C           LAST is the highest element in the d.b NAME,NR=NRUN bank
            LAST = ITABL(JLFIL,LROWS(JLFIL),JLFILR)
            FDBASE = .TRUE.
         ENDIF
      ENDIF
C
C - get the row # IROW  which contains the run # NR
C
      IF (JLFIL.GT.0) THEN
 50      LC = LCOLS(JLFIL)
         LR = LROWS(JLFIL)
C
C     IF the run # IRUN is greater than the last run THEN
C        IF a LFIL bank with a higher bank # exists THEN
C           use this LFIL bank
C        ELSE
C           look at the data base with a IRUN greater than the LAST one
C        ENDIF
C     ELSEIF IRUN is smaller than the 1st one THEN
C        look at the data base
C     ELSE
C        find the right row # in LFIL bank
C     ENDIF
C
         IF (IRUN .GT. ITABL(JLFIL,LR,JLFILR)) THEN
            IF (SMALLER .AND. FDBASE) GOTO 60
            GREATER = .TRUE.
            IF (IW(JLFIL-1) .GT. 0) THEN
               JLFIL = IW(JLFIL-1)
               GOTO 50
            ELSE
               NR = LAST+1
               DIR = 'GE'
               JLFIL = 0
               GOTO 40
            ENDIF
         ELSEIF (IRUN .LT. ITABL(JLFIL,1,JLFIFR)) THEN
            IF (GREATER .AND. FDBASE) GOTO 60
            SMALLER = .TRUE.
            DIR = 'LE'
            JLFIL = 0
            GOTO 40
         ELSE
C
            IROW = LOCTAB (IW(JLFIL+LMHLEN+1),LC,LR,JLFIFR,IRUN)
            IF (IROW.EQ.0) THEN
C           IRUN is outside run range
               JLFIL = 0
            ELSEIF (IROW.LT.0) THEN
C           IRUN is between 1st run of row # IROW and 1st run of
C           row # IROW+1
C           check that it is in the run range of row # IROW
               IROW = -IROW
               IF (IRUN.GT.ITABL(JLFIL,IROW,JLFILR)) JLFIL = 0
            ENDIF
         ENDIF
      ENDIF
C
C - end
C
 60   CONTINUE
      LFILRU = JLFIL
      IF (IGARB.EQ.1) LFILRU = -JLFIL
      END
#endif
