*DK mustrm
      SUBROUTINE MUSTRM
C
C***************************************************************************
C
C T.Wang - 851211
C
C! create normal streamers for a track element
C       that enters one of the sensitive volume of MU detectors.
C
C       The procedures are:
C         (1) Transform track element into local reference system
C             -- the system of the current volume (DFD 4.4.3.);
C         (2) Create track segments in active area of the tubes
C             -- calculate the # of tube hit in each tube plane
C                and the lengths of track segments (DFD 4.4.3.);
C         (3) Create streamers in tube (DFD 4.4.3.);
C
C       Called by MUHIT
C       Calls MUTRSF,MUSTSG,MUSMFR               in this .HLB
C
C***************************************************************************
C
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "bcs.h"
      REAL XLOC(3),DCLOC(3)
C
C       Transform the track element to the local system
C          XLOC  -- the coordinates of track element
C          DCLOC -- the direction cosine of track element
C
      CALL MUTRSF(XLOC,DCLOC)
C
C       Check if the track element is in a special module
C
        CALL MUSPMD(XLOC,IOUFG)
        IF( IOUFG .LT. 0 )GOTO 999
C
C       Create track segment in active area of tubes for two
C       tube plane.
      CALL MUSTSG(XLOC,DCLOC)
C
C       Create streamers for each tube hit
C
      CALL MUSMFR
C
  999 RETURN
      END
#endif
