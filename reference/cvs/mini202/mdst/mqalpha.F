       SUBROUTINE QUINIT
*     ------------------
#ifndef DOC
* Initialisation.

      COMMON / MINMAK / IPRNT,LRUN
      DIMENSION SUMRY(7)
      COMMON / COUNT / ICOUNT
#include "qcde.h"
* Initialise Mini cards file.

      IER = 0
cc      CALL MININC(IER)
cc      IF(IER.GT.0) THEN
cc        PRINT 1001,IER
cc 1001   FORMAT(1X,'IER = ',I3
cc     &     ,'!!!!! no room to book banks for CARDS file !!!!!'
cc     &   //,'!!!!! CARDS file not initialised !!!!!')
cc      ENDIF

* Remove old files.

cc    CALL MINDEL

* Initialise Mini monitoring.

      LRUN = -999
      CALL MINMON(-1,0, SUMRY)
      CALL MINSUM(-1,LRUN,SUMRY)

* Check if we want printout.

c!    IPRNT = 0
      IPRNT = 1
      IF (NLINK('MPRI',0).GT.0) IPRNT = IW(NLINK('MPRI',0)+1)
      CALL HTITLE(' ')

      ICOUNT = 0
      END
#endif
      SUBROUTINE QUEVNT
*     -----------------
#ifndef DOC
* Event processing.

#include "qcde.h"
      COMMON / MINMAK / IPRNT,LRUN
      DIMENSION SUMRY(7)
      CHARACTER*800 ELIST,RLIST
      COMMON / COUNT / ICOUNT
#include "qmacro.h"

* Create Mini-DST for event records.
c     print *,'QUEVNT1',KRUN,KEVT
c     IF (IPRNT.GE.1) WRITE(6,'('' Run/Evt '',2I10)') KRUN,KEVT
c     print *,'QUEVNT'
      CALL MINDST(ELIST,RLIST)
c     print *,'QUEVNT2',KRUN,KEVT

      ICOUNT = ICOUNT + 1
      CALL MINALGO

      CALL MINHIS(IMAQQ)
      CALL MINOUT
      CALL MINMON(0,IMAQQ, SUMRY)
      IF (IPRNT.GE.1) CALL AUBLIS('T')

      END
#endif
      SUBROUTINE QUTERM
*     -----------------
#ifndef DOC
* Event termination.

#include "qcde.h"
      COMMON / MINMAK / IPRNT,LRUN
      DIMENSION SUMRY(7)

* Termination, including printing summary for last run.

      CALL ABUNIT(LDST,LSEL,LSEL2,LMINI,LEDIR)
      CALL BWRITE(IW,LMINI,'0')
      CALL MINMON(+1,0, SUMRY)
      CALL MINSUM(+2,LRUN,SUMRY)
C     CALL BOSTA
C     CALL BOSBK(IW)
      print *, 'HPRINT'
      CALL HPRINT(0)
      print *, 'ACLOSE'
      CALL ACLOSE(0, IER)
      print *, 'STOP'
      STOP

      END
#endif
      SUBROUTINE QUNEWR
*     -----------------
#ifndef DOC
* New run.

#include "qcde.h"
      COMMON / MINMAK / IPRNT,LRUN
      DIMENSION SUMRY(7)

* Print summary for last run, and reinitialise.
* (KRUN = 0 at end of processing.)

      IF (KRUN.EQ.0) RETURN
      IF (KRUN.EQ.LRUN) RETURN
      IF (LRUN.GT.0) THEN
         CALL MINMON(+1,0, SUMRY)
         CALL MINSUM(+1,LRUN,SUMRY)
      ENDIF
      CALL PRRHAH
      LRUN = KRUN

      END
#endif
      SUBROUTINE MINALGO
*     ------------------
#ifndef DOC
* Create thrust DTHR and impact parameter DBTG banks

#include "qcde.h"
      COMMON / MINMAK / IPRNT,LRUN
      DIMENSION SUMRY(7)
      CHARACTER*800 ELIST,RLIST
      COMMON / COUNT / ICOUNT
      DIMENSION CTHR(4)
#include "qmacro.h"

* Fill Alpha banks
c!!!  CALL QFILL
cc      WRITE(6,'('' Count/Run/Evt '',3I10)') ICOUNT,KRUN,KEVT

* Perform Thrust algorithm
      CALL QJOPTR('EF',' ')
      CALL QJTHRU (THRU,'THRUST',KRECO)
      ITHRU = KPDIR('THRUST',KRECO)
      CTHR(1) = QX(ITHRU)
      CTHR(2) = QY(ITHRU)
      CTHR(3) = QZ(ITHRU)
      CTHR(4) = QE(ITHRU)
cc    PRINT *,ICOUNT,THRU,CTHR(1),CTHR(2),CTHR(3),CTHR(4)

* Create and Fill Mini DTHR bank if THRU gt 0.
      IF (THRU .GT. 0.) CALL MINTHR (THRU,CTHR)

* Perform the IMPACT PARAMETER TAG (VDET)
      CALL MDIPBT

* Create and fill the DBTG bank
      CALL MDWRIP(IRET)

      RETURN
      END
#endif
      SUBROUTINE QUSREC
*     -----------------
#ifndef DOC
* Store KSEC and RUNE banks
#include "bcs.h"
C
      CHARACTER*800 WLIST
      CHARACTER*4 NAME
      LOGICAL WANT,CLASS
      DIMENSION CLASS(32)
      INTEGER IDTYP
      COMMON / COUNT / ICOUNT
C
#include "bmacro.h"
C
C++   Add KSEC and RUNE banks to Mini list.
C
      KKSEC = NLINK('KSEC',0)
      KRUNE = NLINK('RUNE',0)
      print *, 'KSEC ',KKSEC,KRUNE
C
      IF(KKSEC.GT.0) CALL MINLIS('KSEC')
      IF(KRUNE.GT.0) CALL MINLIS('RUNE')
      IF(KKSEC.GT.0.AND.KRUNE.GT.0) THEN
        ICOUNT = ICOUNT + 1
        CALL MINOUT
      ENDIF

      RETURN
      END
#endif



