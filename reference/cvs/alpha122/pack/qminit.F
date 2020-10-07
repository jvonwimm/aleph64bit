      SUBROUTINE QMINIT
CKEY INIT /INTERNAL
C----------------------------------------------------------------------
C! initialize analysis job
C! called from QMAIN
C!                                                  H.Albrecht 20.09.88
C!                                          Modified E.Blucher 03.09.89
C!                                          Modified E.Blucher 02.02.92
C!                                          Modified G.Graefe  02.10.95
C!                                          Modified T.Oest    26.07.95
C----------------------------------------------------------------------
#ifndef DOC
      SAVE IOLD,INEW
#include "qcdesh.h"
      COMMON / ALFORMA / LUFORM,IFALARM
      INTEGER ALFMT
      CHARACTER FMT*1
      DATA IOLD,INEW/0,-1/
C----------------------------------------------------------------------
C
C           quasi BLOCK DATA
C
      CALL QDATA
#if defined(UNIX)
C_PVM
C
C   Open log files and initialize PVM if client.
C
      CALL QPVMIS(KUPRNT)
C_PVM
#endif
C
C           interactive job ?
C
      CALL QAINTA
C
C           date,time; print header
C
      CALL QADATI

C          init BOS
C
      CALL QUIBOS(KUPRNT,KUPTER)
      IW(5) = KUCARD
      IW(6) = KUPRNT
C
C           dummy open routine
C
      CALL QUOPEN
C
C Read BOS bank formats :
C
      IND = ALFMT(LUFORM,'ALL ',FMT)
      IF (IND.NE.0) IFALARM = 1
C
C           define NAMe Indices
C
      CALL QMNAMI
C
C           read data cards
C
      CALL QMCARD
C
C          initialize ALPHA
C
      CALL QMALPH
      IF (XCOPYJ .AND. .NOT.XWMINI)  GO TO 90
C
C           read short-term database
C
      CALL QMDBAS
C
C           initialise database
C
      IF (KDEBUG .GT. 0)  CALL QWMESS ('_QMINIT_ Open data base')
      KUCONS = JUNIDB (0)
      CALL AOPDBS (' ',IER)
      IF (IER .NE. 0)
     +  CALL QWSYNT ('0_QMINIT_ Data base cannot be opened')
C    Swap database bank given on data cards with NR=IROLD to IRNEW.
C
      CALL ADBSWP(IOLD,INEW)
C
      CALL MDARD (IW, KUCONS, 'ADBS', 0)
      CALL MDARD (IW, KUCONS, 'ADBL', 0)
      IS = NLINK ('ADBS', 0)
      IL = NLINK ('ADBL', 0)
      IF (IS .GT. 0 .AND. IL .GT. 0)  THEN
        WRITE (KUPRNT, 1001)
     +   IW(IS+3), IW(IS+4), IW(IL+3), IW(IL+4)
      ELSE
        CALL QWMESS
     +   ('_QMINIT_ Data base : ADBS/ADBL is missing')
      ENDIF
C
C          initialize particle table
C
      CALL QMPART
#if defined(UNIX)
C_PVM
C
C   start PVM tasks, OPEN TMP,RUN BUFFER
C
      CALL QPVMIM(KUPRNT)
C
C_PVM
#endif
C
C          initialize histograms
C
      CALL QUIHIS
C
C          initialize formats for mini
C
      CALL MINFMT
C
C          initialize NanoDst if requested
C
      IF (XWNANO) CALL QNINIT
C
C          Initialization for YTOP
C
      CALL YTIJOB
C Initialise EBNEUT package when ENFLW required :
      IF (XFILEF) CALL EBINIT(IER)
C
C         user initialization
C
      IF (XCOPYJ) GO TO 90
      IF (XSYNTX)  THEN
        CALL QWMESS ('_QMINIT_ Syntax flag --> QUINIT not called')
      ELSE
        IF (KDEBUG .GT. 0)  CALL QWMESS ('_QMINIT_ Call QUINIT')
        CALL QUINIT
      ENDIF
C
C         initialisation time
C
   90 CALL QWMESS ('_QMINIT_------- Initialization done --------')
      CALL QWMESS(' ')
      KSTATU = 0
      CALL TIMEX (QTIMEI)
C
C       syntax flag ?
C
      IF (XSYNTX)  CALL QMTERM
     +   ('_QMINIT_ Syntax flag --> no event processing')
C
 1001 FORMAT (' _QMINIT_ Data base version ADBS',2I7,
     +        ', ADBL',2I7)
      END
#endif
