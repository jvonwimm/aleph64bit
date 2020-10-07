      SUBROUTINE VDALIN
C ----------------------------------------------------------------------
C!  smears the VDXY and VDZT coordinates in accordance with
C!         the errors in the alignment quantities.
C!
C -  Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C!
C!  Description
C!  ===========
C!  The alignment procedure for the VDET essentially measures the
C!  position of each wafer with respect to the rest of the detector.
C!  The results are given in the form of 6 quantities - 3
C!  translations and 3 rotations to go from the nominal position to
C!  the measure postion.  The statistical accuracy of these
C!  measurements is given in the 6x6 (symmetric) error matrix (21
C!  numbers) for each wafer.  There is no mathematically rigorous way
C!  of treating the uncertainty in the real position of the wafers -
C!  especially because it is measured with respect to the rest of the
C!  detector which is, in turn, subject to the same uncertainties,
C!  and the global correlations are not stored.
C!  Many possible treatments can be thought about:
C!
C!  This subroutine does treatment B.
C!  (Treatment A => VDOSMR   and   Treatment C => VDWSMR)
C!
C!  B:  Propagate the error analytically from the error matrix on
C!  the six quantities to an arbitray position on the wafer.  Smear
C!  the hits at that position using this error.  (See the subroutine-
C!  header for VDSIGM for the mathematical details.)
C!     While this procedure recovers the position and angular
C!  dependence of the smearing quantities (and results in a near
C!  perfect agreement between the data and MC resolution measurement
C!  as a function of the incident angle), it is still not completely
C!  satisfactory.  For instance, all the hits in one wafer get some
C!  random smearing, while in reality the smearing should be highly
C!  correlated.
C
C
C  Output : smeard VDZT and VDXY banks
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      INTEGER NLINK
      INTEGER KVALC
      LOGICAL FIRST
      SAVE NAVALC
      DATA FIRST/.TRUE./
C
C-- load the right VALC bank from the database
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVALC = NAMIND('VALC')
        KVALC = IW(NAVALC)
        IF (KVALC.LE.0) THEN
          CALL RERROR('VDALIN',1,'No alignment matrix!')
          RETURN
        ENDIF
      ENDIF
      IF (KVALC.LE.0) THEN
        RETURN
      ENDIF
      CALL VDALZT
      CALL VDALXY
      RETURN
      END
#endif
