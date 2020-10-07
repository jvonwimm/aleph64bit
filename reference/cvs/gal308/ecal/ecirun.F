*DK ecirun
      SUBROUTINE ECIRUN
C.----------------------------------------------------------------
C  M.Rumpf dec 85
C                           modified by F.Ranjard - 880308
C!   Modified :- E. Lancon              3-DEC-1990
C! ECAL run initialisation
C     - Called by ASIMOD
C     - Calls     ERDDAF,ECDFRD,ECDFCD,ECDFPA,ECISTA,ECIBUG
C                 ALTELL, GTSTUP
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "iocom.h"
#include "jobcom.h"
      INTEGER GTSTUP, ALGTDB
C ----------------------------------------------------------------
C
C Define formats and name-indices of EC BOS banks
C
      CALL ECNAMI
C
C Initialize Debug Variables for ECAL
C
      CALL ECIBUG
C
C Define Readout Geometry for ECAL
C
      CALL ERDDAF (LRDBIO,IRUNJO,IERR)
      IF (IERR .EQ. 0) THEN
         CALL ALTELL ('ECIRUN: missing data base bank(s)',0,'STOP')
      ENDIF
      CALL ECDFRD
C
C Define Run Conditions for ECAL Analog Signals and Digits
C
      CALL ECDFCD
C
C Define Parameters for ECAL
C
      CALL ECDFPA
C
C?   Get EZTH Bank from data base and put it on run header
      CALL BKFMT('EZTH','2I,(I,3F)')
C    Get ENNO Bank from data base
      IECST = GTSTUP ('EC',IRUNJO)
      IRET = ALGTDB (LRDBIO,'EZTHENNOEDPA',IECST)
      CALL BLIST (IW,'C+','EZTH')
      CALL EUPPAR
C
C Initialize Statistics for ECAL
C
      CALL ECISTA
C
      END
#endif
