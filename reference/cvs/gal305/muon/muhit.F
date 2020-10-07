*DK muhit
      SUBROUTINE MUHIT
C
C*****************************************************************
C
C T.Wang - 851211
C
C! deal with a track element that enters
C       a sensitive volume of MU, it corresponds to
C       the process 'Create MU Signals' in DFD 4.4.3.
C
C       Called by GUSTEP
C       Calls     MUGETC, MUSTRM, MUSGNL    in this .HLB
C
C****************************************************************
C
#ifndef DOC
      SAVE
#include "trkcom.h"
#include "bcs.h"
#include "jobcom.h"
#include "munamc.h"
C
C       Initializion at the biginning of an event
C
      IF( FBEGJO(8) )THEN
         FBEGJO(8) = .FALSE.
         IDRP = IW(NAMUHT)
         IF( IDRP .NE.  0)CALL BDROP(IW,'MUHT')
      ENDIF
C
C       Only deal with the track element entering a new volume
C
      IF(ITRKEL(8) .NE. 1)RETURN
      CALL MUGETC
C
C       Create normal stremers
C
      CALL MUSTRM
C
C       Convert streamers into strip signals
C
      CALL MUSGNL
C
      CALL WDROP (IW,JDMUST)
C
      RETURN
      END
#endif
