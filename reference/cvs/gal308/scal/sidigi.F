*DK sidigi
      SUBROUTINE SIDIGI
C--------------------------------------------------------------
C! Controls digitalisation in SCal
C. - B.Bloch-Devaux        911015
C. - Called by  ASDIGI                           from this .HLB
C. - Calls      SINOIS,SIZSUP,SISIDI,SITRIG,SIBPRN from this Lib
C. -            AUBPRS                             from ALEPHLib
C -------------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "sistat.h"
C ------------------------------------------------------------
C - Add noise and/or cross-talk if requested
      IF (ICSIJO(3).GT.0) CALL SINOIS
C
C - Apply Zero suppression
      CALL SIZSUP
C
C - Create digitalization banks 'SIDI'
      CALL SISIDI
C
C - Form trigger signals
      CALL SITRIG
C
C - Reduce bank size to real one
      CALL AUBPRS ('SIDISIFO')
C
      IF (FHISJO(9)) CALL SIFILH(2)
C
C - Print the output banks for events to be debugged
      IF (NSIPRT.GE.3) THEN
C       Print digi banks
          CALL PRTABL('SIDI',0)
C       Print TRIGGER banks
          CALL PRTABL('SIFO',0)
      ENDIF
  999 RETURN
C
      END
#endif
