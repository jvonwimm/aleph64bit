*DK hcirun
      SUBROUTINE HCIRUN
C-----------------------------------------------------
C
C!     Initialisation Routine for HCAL
C!
C!                      Author : G.Catanesi 87/09/15
C!                     modified: F.Ranjard  88/02/05
C!                     modified: A.Sciaba`  98/04/29
C!   Called by : ASIMOD                             from this .HLB
C!   Calls     : HRDGAL,HCRDCA,HRDTRI,HCNAMI,HCINIT,HCINPA
C!                                                  from this .HLB
C!               HRDDAF                             from ALEPHLIB
C!
C ----------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "iocom.h"
C -----------------------------------------------------------------
C  Reads constants from Data Base
C
      CALL HRDDAF (LRDBIO,IRUNJO,IFLAG)
      IF (IFLAG .EQ. 0) THEN
         CALL ALTELL ('HRDDAF: error when getting geo.banks ',0,'STOP')
      ENDIF
      IF (IPRIJO(7) .NE. 0) CALL HCPRGE
C
C  Reads the Data Base sections used only in Galeph
C
      CALL HRDGAL
C
C  Reads the calibration constants
C
      CALL HCRDCA
C
C  Reads the trigger section of the data base
C
      CALL HRDTRI (LRDBIO,IRUNJO,IFLAG)
      IF (IFLAG .EQ. 0) THEN
         CALL ALTELL ('HRDTRI: error when getting trig.banks ',0,'STOP')
      ENDIF
C
C  If the HCAL set is active  .....
C
      IF (IDETJO(7).NE.0) THEN
C     defines and formattes  read-out bank names
         CALL HCNAMI
C     Initialises variables in work commons
         CALL HCINIT
      ENDIF
C
      END
#endif
