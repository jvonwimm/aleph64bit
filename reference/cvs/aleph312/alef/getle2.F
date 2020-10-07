      SUBROUTINE GETLE2(ELEP,IFLAG)
C----------------------------------------------------------------------
C! Get the "instantaneous" exact LEP energy for LEP 2 runs
CKEY ALEF LEP ENERGY
C                                                  J.Boucrot  26.3.97
C Output arguments :
C  ELEP  = LEP c.m.s. energy, in Gev
C  IFLAG = 1 if ELEP found from official time-slice LEP information
C        = 2 if ELEP found from average run energy
C        = 3 in all other cases (then ELEP is taken from bank 'LFIL')
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "evehjj.h"
#include "rlepjj.h"
#include "rnr2jj.h"
#include "rnl2jj.h"
#include "rnf2jj.h"
      PARAMETER ( M1 = 1000000 , M2 = 10000 )
      LOGICAL TEST
C NRMIN is the first LEP 2 run for which the above banks contain infos
      DATA NRMIN / 41400 /
C EMIN is the LEP energy under which there is no time slice information
      DATA EMIN / 100. /
C inline function:
      TEST(IA,IB,IC) = IA.GE.IB.AND.IA.LT.IC
#include "bmacro.h"
C-----------------------------------------------------------------------
      ELEP=0.
      IFLAG=0
      LDBAS=JUNIDB(0)
C Get run and event number:
      CALL ABRUEV(KRUN,KEVT)
C Only LEP2 runs of 1996 and after are considered :
      IF (KRUN.LT.NRMIN) GO TO 800
C Get time and date of event:
      JEVEH=IW(NAMIND('EVEH'))
      IF (JEVEH.EQ.0) GO TO 800
      KEVETI=IW(JEVEH+JEVETI)
      KEVEDA=IW(JEVEH+JEVEDA)
C Get year IYEAR and time ITIEV (HHMMSS) of the present event:
      ITIEV=KEVETI/100
      IYEAD=KEVEDA/10000
      IF (IYEAD.GE.96) IYEAR=1900+IYEAD
      IF (IYEAD.LT.10) IYEAR=2000+IYEAD
      IDTEV=ITIEV+M1*MOD(KEVEDA,M2)
C
C - Get the bank of LEP2 run-averaged energies: 'RNR2', NR=IYEAR
C
      JRNR2=MDARD(IW,LDBAS,'RNR2',IYEAR)
      IF (JRNR2.EQ.0) GO TO 800
C Find the run-averaged energy for the present run:
      J=LOCTAB(IW(JRNR2+LMHLEN+1),LCOLS(JRNR2),LROWS(JRNR2),JRNRRN,KRUN)
      IF (J.LE.0) GO TO 800
      RELEP=0.
      IF (J.GT.0) RELEP = RTABL(JRNR2,J,JRNREN)
      IF (RELEP.LT.EMIN) GO TO 700
C
C - Get the fill number for this event from the 'RLEP' run header bank:
C
      JRLEP=IW(NAMIND('RLEP'))
      IF (JRLEP.EQ.0) GO TO 700
      IFILL=ITABL(JRLEP,1,JRLELF)
      IF (IFILL.LE.0) GO TO 700
C
C - Get the bank describing the fills : 'RNF2' , NR = IYEAR
C
      JRNF2=MDARD(IW,LDBAS,'RNF2',IYEAR)
      IF (JRNF2.EQ.0) GO TO 700
C Find fill IFILL in this bank:
      K=LOCTAB(IW(JRNF2+LMHLEN+1),LCOLS(JRNF2),LROWS(JRNF2),
     +         JRNFFN,IFILL)
      IF (K.EQ.0) GO TO 700
C Find if the current event is within the time slice of the fill:
      IDFDS=ITABL(JRNF2,K,JRNFDS)
      ITFTS=ITABL(JRNF2,K,JRNFTS)
      IDDST=ITFTS+M1*MOD(IDFDS,M2)
      IDFDE=ITABL(JRNF2,K,JRNFDE)
      ITFTE=ITABL(JRNF2,K,JRNFTE)
      IDDEN=ITFTE+M1*MOD(IDFDE,M2)
      IF (.NOT.TEST(IDTEV,IDDST,IDDEN))  GO TO 700
C
C - Get the bank of LEP2 time-sliced energies: 'RNL2' , NR = IYEAR
C
      JRNL2=MDARD(IW,LDBAS,'RNL2',IYEAR)
      IF (JRNL2.EQ.0) GO TO 700
C Loop on the time slices corresponding to fill IFILL:
      IRLMI=ITABL(JRNF2,K,JRNFFS)
      IRLMX=ITABL(JRNF2,K,JRNFLS)
      DO 100 IRNL2=IRLMI,IRLMX-1
         IDLST=ITABL(JRNL2,IRNL2,JRNLDA)
         ITLST=ITABL(JRNL2,IRNL2,JRNLTI)
         IDDST=ITLST+M1*MOD(IDLST,M2)
         IDLEN=ITABL(JRNL2,IRNL2+1,JRNLDA)
         ITLEN=ITABL(JRNL2,IRNL2+1,JRNLTI)
         IDDEN=ITLEN+M1*MOD(IDLEN,M2)
         IF (TEST(IDTEV,IDDST,IDDEN)) THEN
C Get the "instantaneous" LEP energy for this event:
            ELEP=RTABL(JRNL2,IRNL2,JRNLEN)
            IFLAG=1
            GO TO 999
         ENDIF
 100  CONTINUE
C
C No time-slice information found for this event:
C take the run-averaged one:
C
 700  IFLAG=2
      ELEP=RELEP
      GO TO 999
C
C Neither time-slice nor run-averaged information for this event:
C take the fill-averaged one from bank 'LFIL':
C
 800  IFLAG=3
      ELEP=ALEFIL(KRUN,JLFIL,IRO)
      GO TO 999
 999  RETURN
      END
#endif
