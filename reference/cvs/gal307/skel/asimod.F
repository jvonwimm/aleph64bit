*DK asimod
      SUBROUTINE ASIMOD
C ----------------------------------------------------------------------
C. - F.RANJARD - 850326
C! Initialize all modules (detectors, trigger)
C
C - modified by : F.Ranjard - 911002
C                 introduce SICAL references
C. - called by    ASIJOB                           from this .HLB
C. - calls        xxIRUN                           from this .HLB
C.
C -----------------------------------------------------
#ifndef DOC
#include "iocom.h"
#include "jobcom.h"
#include "alfgeo.h"
C ----------------------------------------------------------------------
C
C - Initialize various detectors
C
      CALL VDIRUN
      CALL ITIRUN
      CALL TPIRUN
      CALL LCIRUN
      CALL SAIRUN
      CALL HCIRUN
      CALL MUIRUN
      CALL ECIRUN
      CALL SIIRUN
C
C - Initialisation of Fxxx output  if required
      IF (FXXXJO) CALL FYIRUN (ALFIEL,FXTKJO,FXSHJO,CUTFJO)
C
C - Initialize the trigger routines
      IF (IPROJO(5).NE.0) THEN
         CALL X1IRUN (IRUNJO)
         CALL X2IRUN (IRUNJO,FHISJO(11),IRET)
         IF (IRET.NE.0) THEN
            CALL ALTELL ('ASIMOD: error in X2LUTS',0,'STOP')
         ENDIF
         IF (IPRIJO(11).EQ.1) CALL X2PRIN ('INIT')
      ENDIF
C
      RETURN
      END
#endif
