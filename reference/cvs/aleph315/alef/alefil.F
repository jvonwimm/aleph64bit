      FUNCTION ALEFIL (IRUN,JLFIL,IROW)
C --------------------------------------------------------------
C - F.Ranjard - 900803
C! Returns the LEP energy, the LFIL index and row # for a given run
CKEY ALEF LEP ENERGY
C - Input    : IRUN    / I     = run number
C - Output   : ALEFIL  / R    = LEP energy
C              JLFIL   / I    = LFIL BOS index
C              IROW    / I    = LFIL row # containing IRUN
C
C - IF (a row exists for this run in LFIL) THEN
C      ALEFIL = LFIL(run,JLFILE)
C   ELSE IF montecarlo (AFID exists) THEN
C      ALEFIL = AFID word(JAFIBE)
C   ELSE IF raw data (RLEP exists) THEN
C      ALEFIL = RLEP word(JRLELE)*2./1000.
C   ELSE no bank
C      ALEFIL = 91.2
C   ENDIF
C   IF ALEFIL<50. THEN ALEFIL=91.2
C ------------------------------------------------------------------
#ifndef DOC
      SAVE NLFIL, NRLEP, ELEPMN, ELEPDF
#include "bcs.h"
#include "afidjj.h"
#include "rlepjj.h"
#include "lfiljj.h"
      DATA NAFID /0/
#include "bmacro.h"
C --------------------------------------------------------------------
C
C - set name-indices
      IF (NAFID .EQ. 0) THEN
         NAFID = NAMIND ('AFID')
         NLFIL = NAMIND ('LFIL')
         NRLEP = NAMIND ('RLEP')
         ELEPDF= 91.2
         ELEPMN= 50.
      ENDIF
C
C - get  the LEP energy
C
      IF (IW(NAFID) .NE. 0) THEN
C     montecarlo run
         JLFIL = 0
         IF (IW(NRLEP) .NE.0) THEN
            ELEP = REAL(ITABL(IW(NRLEP),1,JRLELE)) / 500.
         ELSE
            ELEP = RTABL (IW(NAFID),1,JAFIBE)
         ENDIF
      ELSE
C
C     not a montecarlo run
C     get LFIL row number for this run from data base
         JLFIL = LFILRU (IRUN,IROW)
         IF (JLFIL.EQ.0 .OR. IROW.LE.0) THEN
C        LFIL does not exist or the run does not exist
C        get RLEP from the SOR record
            IF (IW(NRLEP) .NE. 0) THEN
               ELEP = REAL(ITABL(IW(NRLEP),1,JRLELE)) / 500.
            ELSE
C           noway to find ELEP get the default value
               ELEP = ELEPDF
            ENDIF
         ELSE
C        the run exists in LFIL
            ELEP = RTABL(IABS(JLFIL),IROW,JLFILE)
         ENDIF
      ENDIF
      IF (ELEP.LT.ELEPMN) ELEP = ELEPDF
C
      ALEFIL = ELEP
C
      END
#endif
