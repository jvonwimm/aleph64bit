      INTEGER FUNCTION ALGTRO (NAME,NEW,JCOL,IROW)
C --------------------------------------------------------------
C!  get a row of a bank
C - F.Ranjard - 900517
CKEY ALEF GET BANK DA
C
C - Input:    NAME   / A4    = bank name
C             NEW    / INTE  = element which is looking for
C             JCOL   / INTE  = col. number to be considered
C
C - Output:   ALGTRO / INTE  = NAME BOS index
C                              =0 means not enough space
C                              <0 means a garbage collection occurded
C             IROW   / INTE  = row # for the known NEW
C                              0 means NEW<ITABL(JNAME,1,JCOL)
C                              <0 means
C                    ITABL(JNAME,IROW,JCOL)<NEW<ITABL(JNAME,IROW+1,JCOL)
C                               = - LROWS(JNAME) means
C                                NEW>ITABL(JNAME,LROWS(JNAME),JCOL)
C
C ----------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) NAME , DIR*2
#include "bcs.h"
      INTEGER GTDBAS
      LOGICAL GREATER,SMALLER,FDBASE
      SAVE LDBAS, IPRUN
      DATA LAST /0/, NCDAF /0/
#include "bmacro.h"
C ----------------------------------------------------------------
C
C - 1st entry
C
      IF (NCDAF.EQ.0) THEN
         NCDAF = NAMIND ('CDAF')
         LDBAS = JUNIDB(0)
         IPRUN = 0
      ENDIF
C
      NNAME = NAMIND(NAME)
      IGARB = 0
      NR    = NEW
      DIR   = 'LE'
      GREATER = .FALSE.
      SMALLER = .FALSE.
      FDBASE  = .FALSE.
C
C - get NAME bank
C
      JNAME = IW(NNAME)
 10   IF (JNAME.EQ.0) THEN
         IROW = 0
C     get NAME from the data base if any
C
C     1st check validity range of the data base for real data
         IF (IW(NCDAF).EQ.0.AND.IPRUN.NE.NEW.AND.NEW.GE.2001) THEN
            IPRUN = NEW
            IGET = GTDBAS (LDBAS,NEW)
            IF (IGET.NE.0) THEN
               ALGTRO = 0
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
            LAST = ITABL(JNAME,LROWS(JNAME),JCOL)
            FDBASE = .TRUE.
         ENDIF
      ENDIF
C
C - look for the row containing NEW in column # JCOL
C
      IF (JNAME.GT.0) THEN
 20      LC = LCOLS(JNAME)
         LR = LROWS(JNAME)
         IROW = LOCTAB (IW(JNAME+LMHLEN+1),LC,LR,JCOL,NEW)
C
C        IF the element is greater than the highest one in the bank THEN
C           RETURN If the element was smaller than the 1st one in the
C             previous bank which was read from dbase
C           If there is another bank in memory THEN
C              look at it
C           ELSE
C              look at a data base bank with a NR greater than the LAST one
C           ENDIF
C        ELSEIF the element is smaller than the 1st one THEN
C           RETURN If the element was greater than the highest one
C              in the previous bank which was read from  dbase
C           look at the data base
C        ENDIF
C
         IF (IROW.EQ.-LR) THEN
            IF (SMALLER .AND. FDBASE) GOTO 60
            GREATER = .TRUE.
            IF (IW(JNAME-1).GT.0) THEN
               JNAME = IW(JNAME-1)
               GOTO 20
            ELSE
               IF (FDBASE) THEN
                  NR = LAST+1
                  DIR = 'GE'
               ELSE
                  NR = NEW
                  DIR = 'LE'
               ENDIF
               JNAME = 0
               GOTO 10
            ENDIF
         ELSEIF (IROW.EQ.0) THEN
            IF (GREATER .AND. FDBASE) GOTO 60
            SMALLER = .TRUE.
            DIR = 'LE'
            JNAME = 0
            GOTO 10
         ENDIF
      ENDIF
C
C - end
C
 60   CONTINUE
      ALGTRO = JNAME
      IF (IGARB.EQ.1) ALGTRO = -JNAME
      END
#endif
