      INTEGER FUNCTION NAMERU (NAME,IRUN,JCOL,IROW)
C --------------------------------------------------------------
C!  get the row# of a bank which contains IRUN run number in
C   the range defined by col# JCOL and col# JCOL+1
C   ITABL(JNAME,IROW,JCOL)<= IRUN <= ITABL(JNAME,IROW,JCOL+1)
C - F.Ranjard - 931119
CKEY ALEF GET BANK DA
C
C - Input:
C             NAME   / INTE  = bank name
C             IRUN   / INTE  = run # which is looking for
C
C - Output:   NAMERU / INTE  = NAME BOS index
C                              =0 means not enough space
C                              <0 means a garbage collection occurded
C             IROW   / INTE  = row # which contains IRUN run number
C                              0 means IRUN not found
C
C ----------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*) NAME
      CHARACTER*2 DIR
      INTEGER GTDBAS
      LOGICAL GREATER,SMALLER,FDBASE
#include "bcs.h"
      SAVE NCDAF, LDBAS, IPRUN, NRUN, LAST
      DATA NCDAF /0/
#include "bmacro.h"
C ----------------------------------------------------------------
C
C - 1st entry
C
      IF (NCDAF.EQ.0) THEN
        NCDAF = NAMIND('CDAF')
        LDBAS = JUNIDB(0)
        IPRUN = -1
      ENDIF
C
      NNAME = NAMIND(NAME)
      IGARB = 0
      NR = IRUN
      DIR = 'LE'
      GREATER = .FALSE.
      SMALLER = .FALSE.
      FDBASE  = .FALSE.
C
C - get NAME bank
C
      JNAME = IW(NNAME)
   40 IF (JNAME.EQ.0) THEN
C     get NAME from the data base if any
C
C     1st  check validity range of the data base for real data
        IF (IW(NCDAF).EQ.0.AND.IPRUN.NE.IRUN.AND.IRUN.GE.2001) THEN
          IPRUN = IRUN
          IGET = GTDBAS (LDBAS,IRUN)
          IF (IGET.NE.0) THEN
            NAMERU = 0
            RETURN
          ENDIF
        ENDIF
C
        NRUN = NDANR (LDBAS,NAME,DIR,NR)
        IF (NRUN.NE.0) THEN
          JNAME = MDARD (IW,LDBAS,NAME,NRUN)
          IF (JNAME.EQ.0) THEN
            IGARB=1
            CALL BGARB(IW)
            JNAME = MDARD (IW,LDBAS,NAME,NRUN)
            IF (JNAME.EQ.0) GOTO 60
          ENDIF
C           LAST is the highest element in the d.b NAME,NR=NRUN bank
          LAST = ITABL(JNAME,LROWS(JNAME),JCOL+1)
          FDBASE = .TRUE.
        ENDIF
      ENDIF
C
C - get the row # IROW  which contains the run # NR
C
      IF (JNAME.GT.0) THEN
   50   LC = LCOLS(JNAME)
        LR = LROWS(JNAME)
C
C     IF the run # IRUN is greater than the last run THEN
C        IF a NAME bank with a higher bank # exists THEN
C           use this NAME bank
C        ELSE
C           look at the data base with a IRUN greater than the LAST one
C        ENDIF
C     ELSEIF IRUN is smaller than the 1st one THEN
C        look at the data base
C     ELSE
C        find the right row # in NAME bank
C     ENDIF
C
        IF (IRUN .GT. ITABL(JNAME,LR,JCOL+1)) THEN
          IF (SMALLER .AND. FDBASE) GOTO 60
          GREATER = .TRUE.
          IF (IW(JNAME-1) .GT. 0) THEN
            JNAME = IW(JNAME-1)
            GOTO 50
          ELSE
            NR = LAST+1
            DIR = 'GE'
            JNAME = 0
            GOTO 40
          ENDIF
        ELSEIF (IRUN .LT. ITABL(JNAME,1,JCOL)) THEN
          IF (GREATER .AND. FDBASE) GOTO 60
          SMALLER = .TRUE.
          DIR = 'LE'
          JNAME = 0
          GOTO 40
        ELSE
C
          IROW = LOCTAB (IW(JNAME+LMHLEN+1),LC,LR,JCOL,IRUN)
          IF (IROW.EQ.0) THEN
C           IRUN is outside run range
            JNAME = 0
          ELSEIF (IROW.LT.0) THEN
C           IRUN is between 1st run of row # IROW and 1st run of
C           row # IROW+1
C           check that it is in the run range of row # IROW
            IROW = -IROW
            IF (IRUN.GT.ITABL(JNAME,IROW,JCOL+1)) JNAME = 0
          ENDIF
        ENDIF
      ENDIF
C
C - end
C
   60 CONTINUE
      NAMERU = JNAME
      IF (IGARB.EQ.1) NAMERU = -JNAME
      END
#endif
