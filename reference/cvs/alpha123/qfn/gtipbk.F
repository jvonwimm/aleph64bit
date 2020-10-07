      SUBROUTINE GTIPBK(NDIM,ARRAY,BANK,OLDBNK)
CKEY   QIPBTAG / INTERNAL
C ------------------------------------------------------------------------------
C! Read in the given BANK into ARRAY(NDIM,*). 
C  Issue warnings if the obselete OLDBNK is given as cards
C  eg:  Read in calibration from the database bank QIPC, issue warnings if 
C  obselete cards FITP are given or information is missing.
C  Author  Manoj Thulasidas  Dec. 8, 1997
C
C     Output:  FITP  10x6 calibration constants
C ------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
C  Arguments
      INTEGER NDIM
      REAL ARRAY(NDIM,*)
      CHARACTER*4 BANK, OLDBNK
C  Functions
      INTEGER ALGTDB, GTSTUP, NAMIND, JUNIDB
C  Local variables
      INTEGER IBANK, IRET, IVS, NABANK, LUNDB, KBANK,  NAOBNK, KOBNK,
     $   IRUN, IEVT, IDIM, ITYPE, NTYPE
#include "bmacrod.h"
#include "bmacro.h"
C ------------------------------------------------------------------------------
C  Check for calibration cards
      NAOBNK = NAMIND(OLDBNK)
      KOBNK = IW(NAOBNK)
      IF(KOBNK .GT. 0)THEN
        WRITE (IW(6),*)' GTIPBK: WARNING: ',OLDBNK,
     $     ' not supported any more.  Will quit...'
        CALL QMTERM('QMTERM called from GTIPBK:  Unsupported bank used')
      END IF
C  get the run number
C  if the event is not Monte Carlo, do nothing
      CALL ABRUEV(IRUN,IEVT)
C Get database Calibration bank
      IVS  = GTSTUP ('VD',IRUN)
      LUNDB = JUNIDB()
      IRET = ALGTDB(LUNDB,BANK,IVS)
      NABANK = NAMIND(BANK)
      KBANK = IW(NABANK)
      IF(KBANK .GT. 0)THEN
        WRITE (IW(6),*)' GTIPBK: reading the database bank ',BANK
        NTYPE = LROWS(KBANK)
        DO ITYPE=1,NTYPE
          DO IDIM=1,NDIM
            ARRAY(IDIM,ITYPE) = RTABL(KBANK,ITYPE,IDIM)
          END DO
        END DO
      ELSE
        WRITE (IW(6),*)' GTIPBK: Can not read BANK database bank'
        CALL QMTERM(
     $     ' Quit from GTIPBK: Can not read GTIPBK Calib from database')
      END IF
      RETURN
      END
#endif
