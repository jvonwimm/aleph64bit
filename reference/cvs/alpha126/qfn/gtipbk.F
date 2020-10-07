      SUBROUTINE GTIPBK(NDIM,ARRAY,BANK,OLDBNK,NOPT)
CKEY   QIPBTAG / INTERNAL
C-----------------------------------------------------------------------
C! Read in the given BANK into ARRAY(NDIM,*). 
C  Issue warnings if the obselete OLDBNK is given as cards
C  eg:  Read in calibration from the database bank QIPC, issue warnings if 
C  obselete cards FITP are given or information is missing.
C  Author  Manoj Thulasidas  Dec. 8, 1997
C  Modified 17-08-99 P-F Giraud : able to read smearing parameters
C
C  NOPT = 0 for QIPBTAG calibration
C       = 1 for smearing/deletion cards, QIPBTAG1
C       = 2 for smearing/deletion cards, QIPBTAG2
C
C     Output:  FITP  10x6 calibration constants
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
C  Arguments
      INTEGER NDIM, NOPT
      REAL ARRAY(NDIM,*)
      CHARACTER*4 BANK, OLDBNK
C  Functions
      INTEGER ALGTDB, GTSTUP, NAMIND, NLINK, JUNIDB
C  Local variables
      INTEGER IBANK, IRET, IVS, NABANK, LUNDB, KCARD, KBANK,  NAOBNK, 
     &        KOBNK, IRUN, IEVT, IDIM, ITYPE, NTYPE, IPASS
      SAVE IPASS 
      INTEGER KQPYR, IYEAR, KASIM, KIVS
      LOGICAL LSMEAR, ALLEP1
      INTEGER NDANR
      DATA IPASS / 0 /
#include "asimjj.h"
#include "bmacrod.h"
#include "bmacro.h"
C-----------------------------------------------------------------------
C Check if the user is asking for a smearing period that is different from 
C that of the Monte Carlo, change ASIM entry.
      KQPYR = IW(NAMIND('QPYR'))
      IF (KQPYR.NE.0) THEN
        IF (IW(KQPYR).NE.1)
     +       CALL QMTERM('GTIPBK : Missing argument for QPYR')
        IYEAR = IW(KQPYR+1)
        KASIM = IW(NAMIND('ASIM'))
        IF (KASIM.NE.0) IW(KASIM+LMHLEN+JASIYM)=IYEAR
      ENDIF

C  Check for calibration cards
      NAOBNK = NAMIND(OLDBNK)
      KOBNK = IW(NAOBNK)
      IF(KOBNK .GT. 0)THEN
        CALL QMTERM('GTIPBK:  Unsupported bank used')
      END IF
C Check the option and set a logical in case a smearing/deletion card
C is asked
      LSMEAR=.FALSE.
      IF (NOPT.GE.1) THEN
        LSMEAR=.TRUE.
      ELSE
        LSMEAR=.FALSE.
      ENDIF
C Get the run number :
C if the event is not Monte Carlo, do nothing
      CALL ABRUEV(IRUN,IEVT)
      IPASS = IPASS + 1
C Get database Calibration bank
      IVS  = GTSTUP ('VD',IRUN)
      IF (IVS.LT.0) CALL QMTERM(
     +     'QIPBTAG/GTIPBK: Unable to find VDET setup code.')
      IF (LSMEAR) THEN
        IF(ALLEP1(IRUN)) THEN
          KIVS = 100*IVS + NOPT
        ELSE
          KIVS = 100*IVS + 1 + NOPT
        ENDIF
      ELSE
        KIVS = IVS
      ENDIF
      LUNDB = JUNIDB()
      KCARD = NLINK(BANK,-1)
C If the user supplies the 'OQIP' card and forgets to give the smearing
C parameters cardfile :
      IF (KCARD.LE.0.AND.LSMEAR.AND.IW(NAMIND('OQIP')).GT.0)
     +     CALL QMTERM('QIPBTAG/GTIPBK: please supply smearing card '//
     +     'files when using OQIP.')
C Access the database :
      IRET = ALGTDB(LUNDB,BANK,KIVS)
      NABANK = NAMIND(BANK)
      KBANK = IW(NABANK)
      IF(KBANK .GT. 0)THEN
        IF (KCARD.GT.0) THEN
          IF (IPASS.LE.1) WRITE (IW(6),10) BANK
 10       FORMAT(' QIPBTAG/GTIPBK: reading ',A4,' from card file.')
        ELSE
          IF (IPASS.LE.1) WRITE (IW(6),20) BANK,KIVS
 20       FORMAT(' QIPBTAG/GTIPBK: reading database bank ',A4,I5)
          IF (LSMEAR.AND.IW(KBANK-2).NE.KIVS) THEN
            WRITE (IW(6),25) BANK, KIVS
 25         FORMAT(' QIPBTAG/GTIPBK: ERROR: Bank ',A4, I5,' not found.')
            CALL QMTERM('Quit from GTIPBK: cannot read bank')
          END IF
        END IF
        NTYPE = LROWS(KBANK)
        DO ITYPE=1,NTYPE
          DO IDIM=1,NDIM
            ARRAY(IDIM,ITYPE) = RTABL(KBANK,ITYPE,IDIM)
          END DO
        END DO
      ELSE
        WRITE (IW(6),*) 'GTIPBK: Can not read database bank ',BANK
        CALL QMTERM(
     &    ' Quit from GTIPBK: Can not read GTIPBK calib. from database')
      END IF
      RETURN
      END
#endif
