      SUBROUTINE ITCREP
C-----------------------------------------------------------------------
C! DST-DST reprocessing : ITC Reconstruction. Steering routine.
C!
C!    Author      :- J. Sedgbeer 92/02/06
C!
C!   Input:
C!      commons:     /BCS/
C!
C!   Output:
C!      commons:     /ITTIME/
C!
C!   called by : RECEVT
C!
C!   calls :
C!     ICFTOI  Convert FRFT type banks to ITFT type track banks
C!     IREFIN  Refine/improve ITC tracks
C!     ISHUFF  Shuffle/Update FRFT type banks from ITFT type track banks
C!     ITDCOR  Create ITC Track to Digits correlation banks (ALEPHLIB)
C!     ITRMCA  Associate the JULIA ITC tracks with the true MC ones
C!
C!   Libraries required: none
C!
C!======================================================================
#ifndef DOC
#include "bosext.h"
C-----------------------------------------------------------------------
C I/O Commons etc.
#include "bcs.h"
#include "ittime.h"
#include "iflags.h"
C-----------------------------------------------------------------------
C Redo ITC coord. to track assoc. and refit tracks
C
C First Make ITFT type track banks from FRFT type banks
      CALL ICFTOI(0,IER1)
C
C If O.K. then redo coord. assoc.
      IF(IER1.EQ.0) THEN
        CALL IREFIN
C
C Put updated info. into FRFT type banks (from ITFT type banks)
        CALL ISHUFF(0,IER2)
C
        IF(IER2.EQ.1) THEN
          CALL RERROR('ITCREP',5,
     &      ' ISHUFF Missing or empty input banks. Fxxx unchanged ')
        ELSEIF(IER2.EQ.2) THEN
          CALL RERROR('ITCREP',6,
     &      ' ISHUFF more rows in ITFT than FRFT!! Fxxx unchanged ')
        ELSEIF(IER2.EQ.3) THEN
          CALL RERROR('ITCREP',7,'ISHUFF Bos Error. FRFT etc dropped')
        ENDIF
C
C Not O.K.
      ELSEIF(IER1.EQ.1) THEN
C        CALL RERROR('ITCREP',1,'Missing FRFT and/or FRTL. in ICFTOI')
      ELSEIF(IER1.EQ.2) THEN
        CALL RERROR('ITCREP',2,'Missing FICL and FTCL     in ICFTOI')
      ELSEIF(IER1.EQ.3) THEN
        CALL RERROR('ITCREP',3,'Inconsistent banks        in ICFTOI')
      ELSEIF(IER1.EQ.4) THEN
        CALL RERROR('ITCREP',4,'Bos Error                 in ICFTOI')
      ENDIF
C
      END
#endif
