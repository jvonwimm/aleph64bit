*DK asipac
      SUBROUTINE ASIPAC
C ----------------------------------------------------------------------
C. - F.RANJARD - 850326
C! Initialize all packages used by the job
C. - called by    ASIRUN                           from this .HLB
C. - calls        GPHYSI, GHEINI                   from GEANT3.pam
C.                ASBOOK                            from this .HLB
C ---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "iocom.h"
#include "jobcom.h"
#include "alfgeo.h"
#include "gcmulo.h"
C ----------------------------------------------------------------------
C
C - Book histograms
C
      CALL ASBOOK
C
C - Get cross-section and energy loss tables
C
      JGERA = IW(NAMIND('GERA'))
      IF (JGERA.GT.0) THEN
         IF (IW(JGERA).GE.1) GEKMIN = RW(JGERA+1)
         IF (IW(JGERA).GE.2) GEKMAX = RW(JGERA+2)
         IF (IW(JGERA).GE.3) NGEKBIN = IW(JGERA+3)
      ENDIF
      CALL GPHYSI
C
C - initialize GHEISHA
C
      IPACJO = IPACJO +1
      CALL GHEINI
C
C - initialize LUND  (beam energy)
C
      IF(TKINJO.EQ.'LUND' .OR. TKINJO.EQ.'JET') THEN
         IF(BKINJO(4).LE.0.) BKINJO(4) = ALECMS
      ENDIF
C
#if defined(DRAW)
C   Initialize Capture file and standard displays if required
C
      IF (IDS1JO.NE.0) CALL ADINIT
C
#endif
      RETURN
      END
#endif
