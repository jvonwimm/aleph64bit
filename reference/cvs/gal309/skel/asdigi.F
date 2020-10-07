*DK asdigi
      SUBROUTINE ASDIGI
C -------------------------------------------------------------------------
C - F.Ranjard - 851004
C! Digitization steering routine
C - Specific module as xxDIGI should be called if xx-detector has been
C   selected on the SETS data card:
C   in this case IDETJO(i) = i with i in the range [1,9]  and in the
C   following order VD, IT, TP, EC, LC, SA, HC, MU, SI
C
C - modified by : F.Ranjard - 911002
C                 suppress reference to FBEGJO(10)
C                 call SIDIGI if SICAL is there
C
C - Called by   ASPEVE                                 from this .HLB
C - Calls       xxDIGI                                 from this .HLB
C ----------------------------------------------------------------------
#ifndef DOC
#include "iocom.h"
#include "jobcom.h"
      DATA IFI/0/
C  -----------------------------------------------------------------------
C
C - initialize random number if required
C
      IF (IFI.EQ.0) THEN
         IF(IRNDJO(1,4).NE.0) CALL RDMIN (IRNDJO(1,4))
         IFI = 1
      ENDIF
         CALL RDMOUT (IRNDJO(1,4))
C
C - Loop over detectors
C
      IF (IDETJO(1).GT.0) CALL VDDIGI
      IF (IDETJO(2).GT.0) CALL ITDIGI
      IF (IDETJO(3).GT.0) CALL TPDIGI
      IF (IDETJO(4).GT.0) CALL ECDIGI
      IF (IDETJO(5).GT.0) CALL LCDIGI
      IF (IDETJO(7).GT.0) CALL HCDIGI
      IF (IDETJO(8).GT.0) CALL MUDIGI
      IF (IDETJO(9).GT.0) CALL SIDIGI
C
 999  CONTINUE
       RETURN
       END
#endif
