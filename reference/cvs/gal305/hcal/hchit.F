*DK hchit
      SUBROUTINE HCHIT
C --------------------------------------------
C
C! Main routine to process each track element
C!
C!       Author   :G.Catanesi      86/11/24
C!       Mod.     :G.Catanesi      87/11/19
C!       Mod.     :L.Silvestris    18/3/93
C!       input:  McTrackElement stored in common TRKCOM
C!       output bank: HCSE McHcTubeSegments
C!
C!
C!   -Called by : GUSTEP
C!   -Calls     : HCCOIN (detailed geometry)  from this .HLB
C!                HCSHOW (geantino electron param) from this .HLB
C!                HCIEVE, HNEWTK (general) from this .HLB
C!
C -------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "gcking.h"
#include "hccoun.h"
#include "hcloc.h"
#include "tmacrod.h"
      LOGICAL FCHRG,FSENS
#include "tmacrof.h"
C ----------------------------------------------------
C
      FCHRG = TRKELE(14) .NE. 0.
      FSENS = ITRKEL(7).GT.0
C-----------------------------------------------------
C?   evaluate the portion number
C
      IF (TRKVOL(2:2).EQ.'B') THEN
         IHCPOR = 2
      ELSE IF (TRKELE(3).GT.0.) THEN
         IHCPOR = 1
      ELSE
         IHCPOR = 3
      ENDIF
C
C?  initialize same variables at the beginning
C?          of the event
      IF(FBEGJO(7)) THEN
         CALL HCIEVE
      ENDIF
C
      IF(FHCDB2) CALL HNEWTK
C
C============ detailed geometry used in full generator and geantino
C
C - IF geantino particle THEN
C      deposit energy following /CAPANO/ parameters and RETURN
C   ELSE it is a track element
C      IF charged particle in a sensitive volume THEN
C         treat the track element: deposit energy
C      ENDIF
C   ENDIF
C   IF muon THEN RETURN
C   IF charged had. which does not stop with had. mechanism RETURN
C   ENDIF
C
      IF (FTINO) THEN
         CALL HCSHOW
         GOTO 10
      ELSE
         IF (FSENS .AND. FCHRG) THEN
            IF((ITRKEL(8).EQ.0) .AND. (ITRKEL(9).EQ.0))RETURN
            NHCC01 = NHCC01 + 1
            CALL HCCOIN
         ENDIF
      ENDIF
      IF (FMUON) GOTO 10
      IF (FHADC .AND. .NOT.FTRHAD) GOTO 10
C
   10 CONTINUE
      FBEGJO(7) = .FALSE.
C
      RETURN
      END
#endif
