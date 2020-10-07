      SUBROUTINE VDISMR
C ----------------------------------------------------------------------
C!  Sets up the appropriate smearing scheme
C!  depending on the datacard VDSM in the first call
C!  Thereafter, calls the appropriate smearing routine
C!
C -  Author         Manoj Thulasidas  27-MAR-1995
CKEY V_DECK VDET
C
C  Input :
C
C  Output :
C
C ----------------------------------------------------------------------
#ifndef DOC
#include "vdimpl.h"
#include "bcs.h"
      INTEGER NAMIND
      INTEGER KVSMR, NAVSMR, IACT
      INTEGER KVDHT, NAVDHT
      SAVE IACT, NAVDHT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST = .FALSE.
C
C-- control by a data card VSMR = 0/none => call vdwsmr
C--                             = 1      => call vdosmr
C--                             = 2      => call vdalin
C--                             = -1     => no smearing at all
C
        NAVSMR = NAMIND('VSMR')
        KVSMR = IW(NAVSMR)
C
C-- If there is a VSMR card, the set up IACT
C
        IF (KVSMR.NE.0) IACT = IW(KVSMR+1)
C
C-- If the given IACT is not valid, set up the default
C
        IF (IACT.NE.1 .AND. IACT.NE.2 .AND. IACT.NE.-1) IACT = 0
C
C-- Print out the smearing scheme chosen
C
        IF (IACT.EQ.0)
     $    WRITE(IW(6),*)' VDISMR: WAFER (VDWSMR) smearing chosen'
        IF (IACT.EQ.1)
     $    WRITE(IW(6),*)' VDISMR: OLD (VDOSMR) smearing chosen'
        IF (IACT.EQ.2)
     $    WRITE(IW(6),*)' VDISMR: POSITION (VDALIN) smearing chosen'
        IF (IACT.EQ.-1)
     $    WRITE(IW(6),*)' VDISMR: no hit smearing'
C
C-- we have to make sure that the VDHT bank is there
C
        NAVDHT = NAMIND('VDHT')
      ENDIF
      KVDHT = IW(NAVDHT)
      IF (KVDHT.LE.0) THEN
        CALL RERROR('VDISMR',1,' No VDHT bank, skipping the event')
        GOTO 999
      ENDIF
      IF (IACT.EQ.0) CALL VDWSMR
      IF (IACT.EQ.1) CALL VDOSMR
      IF (IACT.EQ.2) CALL VDALIN
 999  RETURN
      END
#endif
