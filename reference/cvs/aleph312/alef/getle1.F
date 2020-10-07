      SUBROUTINE GETLE1(ELEP,IFLAG)
C----------------------------------------------------------------------
C! Get the exact LEP energy for LEP 1 runs
CKEY ALEF LEP ENERGY
C                                                J.Boucrot  26.08.97
C Output arguments :
C  ELEP  = LEP c.m.s. energy, in Gev
C  IFLAG = 2 if ELEP found from average run energy
C        = 3 in all other cases (then ELEP is taken from bank 'LFIL')
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "evehjj.h"
#include "rnr2jj.h"
      PARAMETER ( M1 = 100 , M2 = 10000 )
C NRMIN is the first LEP 1 run for which RNR2 banks contain infos
C NRMAX is the last  LEP 1 run for which RNR2 banks contain infos
      DATA NRMIN,NRMAX / 17891 , 37853 /
#include "bmacro.h"
C-----------------------------------------------------------------------
      ELEP=0.
      IFLAG=0
      LDBAS=JUNIDB(0)
      IYEAR=0
C Get run and event number:
      CALL ABRUEV(KRUN,KEVT)
C Only LEP 1 runs of 1993 to 1995  are considered :
      IF (KRUN.LT.NRMIN.OR.KRUN.GT.NRMAX) GO TO 800
C Get time and date of event:
      JEVEH=IW(NAMIND('EVEH'))
      IF (JEVEH.EQ.0) GO TO 800
      KEVETI=IW(JEVEH+JEVETI)
      KEVEDA=IW(JEVEH+JEVEDA)
C Get year IYEAR of the present event:
      ITIEV=KEVETI/M1
      IYEAD=KEVEDA/M2
      IF (IYEAD.GE.93) IYEAR=1900+IYEAD
      IF (IYEAR.LE.0) GO TO 800
C
C - Get the bank of LEP run-averaged energies: 'RNR2', NR=IYEAR
C
      JRNR2=MDARD(IW,LDBAS,'RNR2',IYEAR)
      IF (JRNR2.EQ.0) GO TO 800
C Find the run-averaged energy for the present run:
      J=LOCTAB(IW(JRNR2+LMHLEN+1),LCOLS(JRNR2),LROWS(JRNR2),JRNRRN,KRUN)
      IF (J.LE.0) GO TO 800
      RELEP=0.
      IF (J.GT.0) RELEP = RTABL(JRNR2,J,JRNREN)
C
 700  IFLAG=2
      ELEP=RELEP
      GO TO 999
C
C No run-averaged information for this event:
C take the fill-averaged one from bank 'LFIL':
C
 800  IFLAG=3
      ELEP=ALEFIL(KRUN,JLFIL,IRO)
      GO TO 999
 999  RETURN
      END
#endif
