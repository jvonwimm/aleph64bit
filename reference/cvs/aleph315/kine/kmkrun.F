      INTEGER FUNCTION KMKRUN (IUCOD,NOTRK,TITLE)
C ---------------------------------------------------------
C - F.Ranjard - 870604
C! Modify KRUN parameters
CKEY KINE KINGAL FILL BANK   /  USER  INTERNAL
C  KRUN bank has been filled in ALKRUN with default parameters
C  which can be overwritten calling this subroutine
C  first Drop KRUN bank, then recreate it
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Name: KMKRUN
C               External References: BDROP(BOS77)
C               Comdecks referenced: BCS
C
C - usage    : JKRUN = KMKRUN (IUCOD,NOTRK,TITLE)
C - input    : IUCOD = user generator code
C              NOTRK = NOtracking marker word
C              TITLE = user run title
C - output   : KMKRUN= KRUN bank index
C                      0 means KRUN does not exist
#ifndef DOC
      SAVE
#include "bcs.h"
      CHARACTER*(*) TITLE
      INTEGER ALKRUN
C ----------------------------------------------------------
C - drop KRUN bank
      CALL BDROP (IW,'KRUN')
C
C - create KRUN bank
      JKRUN = ALKRUN (IUCOD,NOTRK,TITLE)
C
      KMKRUN = JKRUN
C
      END
#endif
