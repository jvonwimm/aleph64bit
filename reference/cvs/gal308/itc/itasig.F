*DK itasig
      SUBROUTINE ITASIG
C.
C...ITASIG  1.01  880407  16:08                        R.Beuselinck
C.
C!  Termination of HITS stage for ITC.
C.  If no track elements have activated ITHIT in this event create a
C.  hit bank containing only noise hits.
C.
C.  Called by: ASASIG                                   from this .HLB
C.      Calls: WBANK, WDROP, BKFRW, BLIST               from BOS77
C.             ITNOIS                                   from this .HLB
C.
C.  Named banks used:
C.  IHIT - created if not existing and reduced
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "bcs.h"
#include "itnamc.h"
#include "itsumc.h"
#include "bmacro.h"
C
C--  If no track segments in the event activated ITHIT allow the
C--  generation of an IHIT bank here with only the noise hits.
C--
      JIHIT = IW(NAIHIT)
      IF (JIHIT.EQ.0) THEN
        CALL ALBOS ('IHIT',0,LMHLEN+LIHIT*MIHIT,JIHIT,IGARB)
        IW(JIHIT+1) = LIHIT
        CALL ITNOIS
      ENDIF
C
C--  Reduce the size of 'IHIT'
C--
      CALL AUBPRS ('IHIT')
      CALL BLIST (IW,'E+','IHIT')
C
C--  Fill summary statistics.
C--
      NEVHIT = NEVHIT + 1
      JIHIT = IW(NAIHIT)
      NHCUIT = LROWS(JIHIT)
C
C--   Print out of IHIT
C--
      IF (FDEBJO .AND. IPRIJO(2).EQ.1) THEN
         CALL ITPRHT
      ENDIF
C
  999 CONTINUE
      END
#endif
