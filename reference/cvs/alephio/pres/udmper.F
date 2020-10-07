      SUBROUTINE UDMPER (IRTYP,TLIST,ERLIST,IFLG)
C -------------------------------------------------------
C! user routine, treat decompression errors, set error=0
C - F.Ranjard - 900511
CKEY PRESS DECOMPRESS ERROR / USER
C
C - input    : IRTYP   / INTE          = record type
C                                        1 event, 2 run, 3 unknown
C              TLIST   / CHARACTER*1   = input list
C              ERLIST  / CHARACTER*(*) = error list
C              IFLG    / INTE          = error flag
C
C - output   : IFLG    / INTE           = 0 keep the record anyway
C
C --------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) TLIST,ERLIST
#include "bcs.h"
C
C ----------------------------------------------------------------
      IF (IFLG .EQ. 0) RETURN
C
C - get run and event number
C
      IRUN = 0
      IEVT = 0
      IF (IRTYP.EQ.1) THEN
         JEVEH = IW(NAMIND('EVEH'))
         IF (JEVEH.NE.0) THEN
            IRUN = IW(JEVEH+2)
            IEVT = IW(JEVEH+6)
         ENDIF
      ELSEIF (IRTYP.EQ.2) THEN
         JRUNR = IW(NAMIND('RUNR'))
         IF (JRUNR.NE.0) THEN
            IRUN = IW(JRUNR+2)
         ENDIF
      ENDIF
C
C - NER = number of bank names where no compression was done. Because
C         of errors or lack of space in the BOS array
C
      NER = IFLG/1000
C
C - IER = number of bank names where data check failed. Decompressed
C         banks produced
C

      IER = MOD (NER,1000)
C
C - decode error list
C
      WRITE (IW(6),800) IRTYP,IRUN,IEVT,TLIST,IFLG,NER,ERLIST
 800  FORMAT(1X,  '+++UDMPER+++ ',
     &            ' record type= ',I2,
     &            ' run ',I8, ' event ',I8,
     &            ' LIST= ',A,' IFLG= ',I8,
     &            ' the ERLIST contains ',I3,' banks'/14X,
     &            ' list of non decompressed banks= ',A)
C
C- reset error flag to 0
C
      IFLG = 0
C
      END
#endif
