*DK gutrak
      SUBROUTINE GUTRAK
C -------------------------------------------------------------------
C - F.Ranjard - 870331
C! Follow one particle through the apparatus using Geant3
C - IF  tracking type .eq. NOtracking marker word  THEN  do NOT track
C   IF the particle comes from the generator and has been decayed in
C      the generator THEN do NOT track
C   Otherwise DO tracking  --> CALL GTRACK
C   ENDIF
C - called by GTREVE                                  from GEANT3lib
#ifndef DOC
C --------------------------------------------------------
      SAVE
#include "kincom.h"
#include "gckine.h"
#include "bcs.h"
      EXTERNAL NLINK
#include "bmacro.h"
#include "kmacro.h"
C -------------------------------------------------------------------
C - IF tracking type .EQ. NOtracking marker word  THEN  do NOT track
      IF (IGTRTY .EQ. NOTRKI) GOTO 999
C - IF part. comes from the generator and has been decayed THEN do NOT tr
      IF (IGSTAK.EQ.0 .AND. IGTRA.LE.NITRKI) THEN
         JKINE = NLINK ('KINE',IGTRA)
         KLIS = KLISVK(JKINE)
         IF ((IW(JKINE+3).GT.1) .AND. (IW(KLIS+2).LE.NIVXKI)) GOTO 999
      ENDIF
C
      CALL GTRACK
C
 999  CONTINUE
      END
#endif
