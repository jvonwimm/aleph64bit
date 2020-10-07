      SUBROUTINE MUREDO(IER)
C----------------------------------------------------------------------
C
CKEY MUONID MUON RERUN / INTERNAL
C
C!
C!   Authors   :- G.Taylor
C!
C!   This routine can be called to rerun the muon identification part
C!   of JULIA outside the context of JULIA. Must call MUNEWR every time
C!   a new run is encountered.
C!
C!           IER /I = Error flag
C!                  = 0 ok
C!                  = 2 bad run for hcal digital pattern
C!                  > 10  problem in hrhtub
C!                  > 100 problem in mrmhit
C!                  > 1000 problem in muido
C!
C!======================================================================
#ifndef DOC
#include "bcs.h"
      LOGICAL HDODGY
#include "bmacro.h"
C----------------------------------------------------------------------
      IER=0
      CALL ABRUEV (KRUN,KEVE)
      IF (HDODGY(KRUN)) THEN
C
C This run is bad for muons. Set error flag.
C
        IER = 2
      ENDIF

      CALL BDROP(IW,'HROAMUEXHMADMTHRMHITMCADMUIDTREXD4CD')

      IF (KRUN.LT.2000) THEN
        CALL HRHTUB(IERT)
        IF (IERT.NE.0) IER = IER+10*IERT
      ENDIF
C
      CALL FTRACK
      CALL HMFIND
      IF (KRUN.LT.2000) THEN
        CALL MPREDM
      ELSE
        CALL MPREDG
      ENDIF
      IF (KRUN.LT.2000) THEN
        CALL MRMHIT(IERT)
        IF (IERT.NE.0) IER = IER+100*IERT
      ENDIF
      CALL MUASS
      CALL MUIDO(IERT)
      IF (IERT.NE.0) IER = IER+1000*IERT
      CALL BLIST(IW,'E+','MUIDD4CD')

      RETURN
      END
#endif
